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
N <- ceiling(as.numeric(args[3]))   # Number of nodes
beta_epid <-  as.numeric(args[4]) # Infection rate
gamma_epid <-  as.numeric(args[5]) # Recovery rate
nb_init <-  ceiling(as.numeric(args[6])) # Nb of initial patients
power_pa <- as.numeric(args[7]) # parameter of the PA graph
heterogeneity_rates <- args[8] # are the rates homogeneous?
steps <- ceiling(as.numeric(args[9]))
diffuse <- ceiling(as.numeric(args[10]))
propagation <- args[11]
alpha_fp <- as.numeric(args[12])

p_norm <- 1
mode <- "denoise"
do_plot <- FALSE #TRUE

lambdas <- 10^(seq(from = -5, to = -1, length.out = 30))

res <- c()
for (exp in 1:200) {
  # Create random graph
  g <- sample_pa(N, power = power_pa, directed = FALSE)
  n <- vcount(g)
  if (do_plot) {
    layout <- layout_with_fr(g)
    plot(g, layout = layout, vertex.size = 4,
        edge.arrow.size = 0, vertex.label = NA,
        vertex.color = V(g)$color)
  }
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
  graph_attributes <- get_edge_incidence(g, beta_v, graph = "PA",
                                         weight = 1)
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
                             steps = diffuse,
                             propagate = propagation,
                             alpha_fp = alpha_fp)
  if (do_plot) {
    source("experiments/plot_results.r")
    plot_results_on_graph(g, state$track_state, 1:ncol(state$track_state),
                          "Time step",
                          paste0("plot-pa-power", power_pa, 
                                 "-algo-", propagation))

  }
  store_solutions <- matrix(0, nrow = n, ncol = length(lambdas))
  lambda_it <- 1
  for (lambda in lambdas) {
    if (mode == "predict") {
      y_prob <- y_init
    } else {
      y_prob <- state$y_observed
    }
    p_hat <- tryCatch(
      cvx_solver(y_prob,
                 graph_attributes$Gamma,
                 lambda, p_norm = p_norm),
      error = function(err) {
          # Code to handle the error
          cat("Error occurred while running CVXR:", conditionMessage(err), "\n")
          # Return a default value or NULL to continue with the rest of the code
          return(NULL)
        }
      )
    if (is.null(p_hat) == FALSE) {
      p_hat[which(p_hat < 0)] <- 0
      p_hat[which(p_hat > 1)] <- 1
      store_solutions[, lambda_it] <- p_hat
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
      res_temp["n"] <- vcount(g)
      res_temp["power_pa"] <- power_pa
      res_temp["steps"] <- steps
      res_temp["diffuse"] <- diffuse
      res_temp["propagation"] <- propagation
      res_temp["mode"] <- mode
      res_temp["heterogeneity_rates"] <- heterogeneity_rates
      res_temp["nb_init"] <- nb_init
      res_temp["p_norm"] <- p_norm
      res_temp["alpha_fp"] <- alpha_fp

      # Propagate solution
      prop_sol <- propagate_solution(graph_attributes$W, p_hat,
                                     state$beta_v,
                                     state$gamma_v, steps)
      # Propagate real data
      prop_truth <- propagate_solution(graph_attributes$W, state$true_p,
                                       state$beta_v, state$gamma_v, steps)

      # Propagate benchmark
      prop_benchmark <- propagate_solution(graph_attributes$W, state$y_observed,
                                          state$beta_v, state$gamma_v, steps)
      # Compare the two
      res_temp[paste0("l1_error_", 1)] <- mean(abs(p_hat - state$true_p))
      res_temp[paste0("l2_error_", 1)] <- mean((p_hat - state$true_p)^2)
      res_temp[paste0("benchmark_l1_error_", 1)] <- mean(abs(p_hat - state$y_observed))
      res_temp[paste0("benchmark_l2_error_", 1)] <- mean((p_hat - state$y_observed)^2)
      for (it in 1:steps){
        res_temp[paste0("l1_error_", it + 1)] = mean(abs(prop_truth[[it]] - prop_sol[[it]]))
        res_temp[paste0("l2_error_", it + 1)] = mean((prop_truth[[it]] - prop_sol[[it]])^2)
        res_temp[paste0("benchmark_l1_error_", it + 1)] = mean(abs(prop_truth[[it]] - prop_benchmark[[it]]))
        res_temp[paste0("benchmark_l2_error_", it + 1)] = mean((prop_truth[[it]] - prop_benchmark[[it]])^2)
      }
      # add to list of res
      if (is.null(res)) {
        res <- data.frame(res_temp)
      } else {
        res <- rbind(res, res_temp)
      }
      write_csv(x = res,
                file = paste0("experiments/results/pa_graph/", result_file))
    }
    lambda_it <- lambda_it + 1
  }
  if (do_plot) {
    plot_results(res)
    plot_results_on_graph(g, state$store_solutions, lambdas,
                          "lambda = ",
                          paste0("solution-plot-pa-power", pa_power,
                                 "-algo-", propagation))
  }
}
