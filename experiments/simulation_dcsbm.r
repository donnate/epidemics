library(tidyverse)
library(igraph)
library(fastRG)
#setwd("~/Documents/epidemic_modelling/")
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
dc_heterogeneity <- args[7] # parameter of the DC-SBM graph
heterogeneity_rates <- args[8] # are the rates homogeneous?
steps <- ceiling(as.numeric(args[9]))
p_norm <- args[10]
proba_between <- (as.numeric(args[11]))
proba_within <- (as.numeric(args[12]))
nb_blocks  <- ceiling(as.numeric(args[13]))
diffuse <- ceiling(as.numeric(args[15]))
if (p_norm != "inf"){
  p_norm <- ceiling(as.numeric(p_norm))
}
do_plot <- FALSE

#nb_blocks <-  6
#proba_between <- 0.01
#proba_within <- 0.1
proba_between <- 0.001
proba_within <- 0.01
B <-  matrix(proba_between, nb_blocks, nb_blocks)
diag(B) <- rep(proba_within, nb_blocks)


if (dc_heterogeneity == "none"){
  dc_vector = rep(1, N)
} else{
  dc_vector = rexp(1/as.numeric(dc_heterogeneity), n=N)
}

lambdas <- 10^(seq(from = -5, to = -1, length.out = 30))


res <- c()
for (exp in 1:100){
  # Create random graph
  dcsbm_graph <- dcsbm(
    n = NULL,
    theta = dc_vector,
    B = B,
    sort_nodes = TRUE,
    allow_self_loops = FALSE,
    poisson_edges = FALSE
  )

  edgelist <- sample_edgelist(dcsbm_graph)
  g <- igraph::graph_from_edgelist(as.matrix(edgelist), directed=FALSE)
  #V(g)$z = dcsbm_graph$z
  g = subgraph(g, vids = (1:vcount(g))[components(g)$membership == 1])
  n <- vcount(g)

  if (do_plot) {
    layout <- layout_with_fr(g)
    plot(g, layout = layout, vertex.size = 4,
        edge.arrow.size = 0, vertex.label = NA)
  }

  # # Assign initial patients
  y_init <- rep(0, n)
  subject_0 <- sample(1:n, nb_init)
  y_init[subject_0] <- 1

  print(c(n, vcount(g)))
  # Record statistics on the initial patients
  d <- degree(g, v = subject_0,
              mode = "total", loops = FALSE,
              normalized = FALSE)
  btw <- betweenness(g, v = subject_0)
  cls <- closeness(g, v = subject_0)

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


  
  state <- simulate_epidemic(graph_attributes$W,
                             y_init = y_init,
                             beta_v = beta_v,
                             gamma_v = gamma_v,
                             steps = diffuse)
  
  if (do_plot) {
    library(viridis)
    library(magick)
    library(ggraph)
    k= 100
    color_palette <- viridis(k, option = "C")
    for (t in 1:ncol(state$track_state)){
      sizes = sapply(state$track_state[,t], function(x){ifelse(x < 1e-5, 0.5, 1)})
      V(g)$size <- sizes
      V(g)$color = sapply(state$track_state[,t] , function(x){ifelse(x>0, min(log(1/x),20), 20)})/20
      plot <- ggraph(g, layout = layout) +
        geom_node_point(aes(colour = color), size=2) +        # Add nodes as points
        geom_edge_link(edge_alpha = 0.15) +         # Add edges as links
        scale_colour_gradient(low = "red", high = "navy", limits = c(0,1))+
        theme_bw()               # Remove axis labels and gridlines
      title_plot = paste0("plot-dcsbm-graph-heterogeneity", dc_heterogeneity, "-t-",t,  ".png")
      print(plot)
      ggsave(title_plot, plot = plot, width = 9, height = 9)
      # Load an image
      image <- image_read(title_plot)
      # Resize it to even dimensions
      image_resized <- image_resize(image, "2832x2832")
      image_write(image_resized, paste0("resized", title_plot))
      Sys.sleep(1)
    }
    
  }
  
  #graph_attributes$W[subject_0, neighbors]
  for (lambda in lambdas) {
    print(c(lambda, p_norm))
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
      p_hat[which(p_hat >1)]=1
     # p_hat[which(abs(p_hat) <1e-7)]=0
      
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
      res_temp["q25_btw"] <- quantile(btw, 0.25,na.rm=TRUE)
      res_temp["q75_btw"] <- quantile(btw, 0.75,na.rm=TRUE)
      res_temp["min_btw"] <- min(btw)
      res_temp["max_btw"] <- max(btw)
      res_temp["average_cls"] <- mean(cls,na.rm=TRUE)
      res_temp["median_cls"] <- median(cls,na.rm=TRUE)
      res_temp["q25_cls"] <- quantile(cls, 0.25,na.rm=TRUE)
      res_temp["q75_cls"] <- quantile(cls, 0.75,na.rm=TRUE)
      res_temp["min_cls"] <- min(cls,na.rm=TRUE)
      res_temp["max_cls"] <- max(cls,na.rm=TRUE)
      
      res_temp["exp"] <- exp
      res_temp["beta_epid"] <- beta_epid
      res_temp["gamma_epid"] <- gamma_epid
      res_temp["n"] <- n
      res_temp["dc_heterogeneity"] <- dc_heterogeneity
      res_temp["nb_blocks"] <- nb_blocks
      res_temp["p_between"] <- proba_between
      res_temp["p_within"] <- proba_within
      res_temp["steps"] <- steps
      res_temp["diffuse"] <- diffuse
      res_temp["heterogeneity_rates"] <- heterogeneity_rates
      res_temp["nb_init"] <- nb_init
      res_temp["p_norm"] <- p_norm

      # Propagate solution
      prop_sol <- propagate_solution(graph_attributes$W, p_hat, 
                                     state$beta_v, 
                                     state$gamma_v, steps)
      # Propagate real data
      prop_truth <- propagate_solution(graph_attributes$W, state$true_p, 
                                      state$beta_v, state$gamma_v, steps)
      # Compare the two
      for (it in 1:steps){
        res_temp[ paste0("l1_propagated_error_", it)] = mean(abs(prop_truth[[it]] - prop_sol[[it]]))
        res_temp[ paste0("l2_propagated_error_", it)] = mean((prop_truth[[it]] - prop_sol[[it]])^2)
      }

      # add to list of res
      res <- rbind(res, res_temp)
      write_csv(x = res, file=paste0("experiments/results/dcsbm_graph/", result_file))

    }
    print(lambda)
  }
  print(paste0("Finished experiment ", exp))
}
