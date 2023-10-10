library(tidyverse)
library(igraph)
source("graph_utils.R")
source("experiments/evaluate_solution.R")
source("experiments/simulate_epidemic.R")
source("r/solvers/cvx_solver.R")

# Parameters
args <- commandArgs(trailingOnly = TRUE)
seed <-  ceiling(as.numeric(args[1]))
result_file <- args[2]
n <- ceiling(as.numeric(args[3]))   # Number of nodes
beta_epid <-  as.numeric(args[4]) # Infection rate
gamma_epid <-  as.numeric(args[5]) # Recovery rate
nb_init <-  ceiling(as.numeric(args[6])) # Nb of initial patients
power_pa <- as.numeric(args[7]) # parameter of the PA graph
heterogeneity_rates <- args[8] # are the rates homogeneous?
steps <- ceiling(as.numeric(args[9]))
p_norm <- args[10]
if (p_norm != "inf"){
  p_norm <- ceiling(as.numeric(p_norm))
}
do_plot <- FALSE


res <- c()
for (exp in 1:100){
  # Create random graph
  g <- sample_pa(n, power = power_pa, directed = FALSE)
  if (do_plot) {
    layout <- layout_with_fr(g)
    plot(g, layout = layout, vertex.size = 4,
        edge.arrow.size = 0, vertex.label = NA)
  }


  #neighbors <- as.numeric(neighborhood(g, nodes = c(subject_0), mindist=1)[[1]])
  #neighbors2 <- as.numeric(neighborhood(g, order=2, nodes = c(subject_0), mindist=2)[[1]])
  #print(neighbors)
  #edges_lst = which(graph_attributes$Gamma[, subject_0]!=0)
  #graph_attributes$Gamma[edges_lst, which(graph_attributes$Gamma[edges_lst, ]!=0, arr.ind=TRUE)[,2]]
  # Simulate one step of epidemic propagation

  ### Turn the infection rates into a vector
  if (is.null(heterogeneity_rates) || heterogeneity_rates == "none") {
    beta_v <- rep(beta_epid, n)
    gamma_v <- rep(gamma_epid, n)
  } else {
    if (heterogeneity_rates == "uniform") {
      beta_v <- runif(beta_epid / 2, min(3 * beta_epid / 2, 1), n = n)
      gamma_v <- runif(gamma_epid / 2, min(3 * gamma_epid / 2, 1), n = n)
    } else {
      beta_v <- rexp(1 / beta_epid, n = n)  ### turn into a vector
      gamma_v <- rexp(1 / gamma_epid, n = n)
    }
  }
  graph_attributes <- get_edge_incidence(g, beta_v, graph = "PA", weight=1)


  # Assign initial patients
  y_init <- rep(0, n)
  subject_0 <- sample(1:n, nb_init)
  y_init[subject_0] <- 1

  # Record statistics on the initial patients
  d <- degree(g, v = subject_0,
              mode = "total", loops = TRUE,
              normalized = FALSE)
  btw <- betweenness(g, v = subject_0)
cls <- closeness(g, v = subject_0)


  state <- simulate_epidemic(graph_attributes$W,
                             y_init = y_init,
                             beta_v = beta_v,
                             gamma_v = gamma_v,
                             steps = steps)
  
  #graph_attributes$W[subject_0, neighbors]
  for (lambda in 10^(seq(from = -5, to = 1, by = 0.25))) {
    
     p_hat <- tryCatch(
        cvx_solver(y_init,
                   graph_attributes$Gamma,
                   lambda, p_norm=p_norm),
        error = function(err) {
          # Code to handle the error (e.g., print an error message, log the error, etc.)
          cat("Error occurred while running CVXR:", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        }
      )
    if (is.null(p_hat) == FALSE) {
      p_hat[which(p_hat <0)]=0
      p_hat[which(abs(p_hat) < 1e-7)] = 0
      
      res_temp <- evaluate_solution(state$y_observed,
                                    p_hat,
                                    state$true_p,
                                    graph_attributes$Gamma)
      res_temp["lambda"] <- lambda

      # add the init statistics
      res_temp["average_degree"] <- mean(d)
      res_temp["median_degree"] <- median(d)
      res_temp["q25_degree"] <- quantile(d, 0.25)
      res_temp["q75_degree"] <- quantile(d, 0.75)
      res_temp["min_degree"] <- min(d)
      res_temp["max_degree"] <- max(d)
      res_temp["average_btw"] <- mean(btw)
      res_temp["median_btw"] <- median(btw)
      res_temp["q25_btw"] <- quantile(btw, 0.25)
      res_temp["q75_btw"] <- quantile(btw, 0.75)
      res_temp["min_btw"] <- min(btw)
      res_temp["max_btw"] <- max(btw)
      res_temp["average_cls"] <- mean(cls)
      res_temp["median_cls"] <- median(cls)
      res_temp["q25_cls"] <- quantile(cls, 0.25)
      res_temp["q75_cls"] <- quantile(cls, 0.75)
      res_temp["min_cls"] <- min(cls)
      res_temp["max_cls"] <- max(cls)
      
      res_temp["exp"] <- exp
      res_temp["beta_epid"] <- beta_epid
      res_temp["gamma_epid"] <- gamma_epid
      res_temp["n"] <- n
      res_temp["power_pa"] <- power_pa
      res_temp["steps"] <- steps
      res_temp["heterogeneity_rates"] <- heterogeneity_rates
      res_temp["nb_init"] <- nb_init
      res_temp["p_norm"] <- p_norm

      # Propagate solution
      prop_sol <- propagate_solution(graph_attributes$W, p_hat, state$beta_v, 
                                    state$gamma_v, 20)
      # Propagate real data
      prop_truth <- propagate_solution(graph_attributes$W, y_init, 
                                      state$beta_v, state$gamma_v, 20)
      # Compare the two
      for (it in 1:20){
        res_temp[ paste0("l1_propagated_error_", it)] = mean(abs(prop_truth[[it]] - prop_sol[[it]]))
        res_temp[ paste0("l2_propagated_error_", it)] = mean((prop_truth[[it]] - prop_sol[[it]])^2)
      }

      # add to list of res
      res <- rbind(res, res_temp)
      write_csv(x = res, file=paste0("experiments/results/pa_graph/", result_file))

    }


  }
}
