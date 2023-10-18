library(tidyverse)
library(igraph)
#library(ggraph)
#library(viridis)
#library(magick)
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
proba_er <- as.numeric(args[7]) # parameter of the ER graph
heterogeneity_rates <- args[8] # are the rates homogeneous?
steps <- ceiling(as.numeric(args[9]))
p_norm <- args[10]
diffuse <-  ceiling(as.numeric(args[11]))

if (p_norm != "inf"){
  p_norm <- ceiling(as.numeric(p_norm))
}
do_plot <- FALSE

lambdas <- 10^(seq(from = -7, to = -1, length.out = 50))
res <- c()
for (exp in 1:100){
    # Create random graph
  g <- sample_gnm(n, m = rbinom(1, prob=proba_er, size=n^2-n), directed = FALSE)
  if (do_plot) {
    layout <- layout.mds(g)
    plot(g, layout = layout, vertex.size = 4,
        edge.arrow.size = 0, vertex.label = NA)
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
                             steps = diffuse, ## for now
                             propagate = "true_p")
  
  if (do_plot) {
    k <- 100
    color_palette <- viridis(k, option = "C")
    for (t in 1:ncol(state$track_state)){
      V(g)$color <- sapply(state$track_state[, t], 
                          function(x){ ifelse(x>0, 
                                             min(log(1 / x),20),
                                             20)
                          })/20
      plot <- ggraph(g, layout = layout) +
        geom_node_point(aes(colour = color), size=2) + 
        geom_edge_link(edge_alpha=0.15) +         # Add edges as links
        #scale_colour_brewer(palette = "PuBuGn", direction = -1) + 
        scale_colour_gradient(low = "red", high = "blue", 
                              limits = c(0,1)) + 
        guides(color = "none")+
        theme_bw()               # Remove axis labels and gridlines
      print(plot)
      ggsave(paste0("plot-er-graph-proba-",proba_er, "-", t,  ".png"), 
            plot = plot, width = 9, height = 9)
      # Load an image
      image <- image_read(paste0("plot-er-graph-proba-",
                                proba_er, "-", t,  ".png"))
      # Resize it to even dimensions
      image_resized <- image_resize(image, "2832x2832")
      image_write(image_resized, 
                  paste0("resized-plot-er-graph-proba-",
                         proba_er, "-", t,  ".png"))
      Sys.sleep(1)
    }
  }

  store_solutions = matrix(0, nrow=n, ncol=length(lambdas))
  lambda.it = 1
  for (lambda in lambdas) {
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
      p_hat[which(p_hat < 0)] <- 0
      p_hat[which(p_hat > 1)] <- 1
      store_solutions[, lambda.it] = p_hat
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
      res_temp["proba_er"] <- proba_er
      res_temp["steps"] <- steps
      res_temp["diffuse"] <- diffuse
      res_temp["heterogeneity_rates"] <- heterogeneity_rates
      res_temp["nb_init"] <- nb_init
      res_temp["p_norm"] <- p_norm

      # Propagate solution
      prop_sol <- propagate_solution(graph_attributes$W, p_hat,
                                     state$beta_v, state$gamma_v, steps)
      # Propagate real data
      prop_truth <- propagate_solution(graph_attributes$W, state$true_p,
                                       state$beta_v, state$gamma_v, steps)
      # Compare the two
      res_temp[paste0("l1_error_", 1)]  <- mean(abs(p_hat - state$true_p))
      res_temp[paste0("l2_error_", 1)]  <- mean((p_hat - state$true_p)^2)
      for (it in 1:steps){
        res_temp[paste0("l1_error_", it + 1)] <- mean(abs(prop_truth[[it]] - prop_sol[[it]])) # nolint
        res_temp[paste0("l2_error_", it + 1)] <- mean((prop_truth[[it]] - prop_sol[[it]])^2) # nolint
      }
      res <- rbind(res, res_temp)
      write_csv(x = res, file=paste0("experiments/results/er_graph/", result_file))
    }
    lambda.it <- lambda.it + 1
  }
  
  if (do_plot) {
    k= 100
    library(viridis)
    library(magick)
    #color_palette = colorRampPalette(c("black", "red"))(n/k)
    color_palette <- viridis(k, option = "C")
    for (t in 1:length(lambdas)){
      #sizes = sapply(store_solutions[,t], function(x){ifelse(x < 1e-5, 0.5, 1)})
      #V(g)$size <- sizes
      V(g)$color <- color_palette[cut(sapply(store_solutions[,t] , 
                                  function(x){ifelse(x>0, min(log(1/x),20), 20)})/20,
                                      breaks = length(color_palette), 
                                      include.lowest = TRUE)]
      #V(g)$color <- cut(sapply(state$track_state[,t] , function(x){ifelse(x>0, min(log(1/x),20), 20)})/20,
      #                                breaks = length(color_palette), 
      #                                include.lowest = TRUE)
      #V(g)$color <- sapply(state$true_p, function(x){color_palette[which.min( abs(x * (n/k) - (1:(n/k))))]})
      V(g)$color = sapply(store_solutions[,t] , function(x){ifelse(x>0, min(log(1/x),20), 20)})/20
      #layout <- layout_with_fr(g)
      plot <- ggraph(g, layout = layout) +
        geom_node_point(aes(colour = color), size=2) +        # Add nodes as points
        geom_edge_link(edge_alpha=0.15) +         # Add edges as links
        #scale_colour_brewer(palette = "PuBuGn", direction = -1) + 
        scale_colour_gradient(low = "red", high = "navy", limits = c(0,1))+
        theme_bw() +
        ggtitle(paste0("lambda = ", lambdas[t]))             # Remove axis labels and gridlines # nolint: line_length_linter.
      print(plot)
      title_plot = paste0("plot-er-graph", proba_er, "-solution" ,t,  ".png")
      ggsave(title_plot, plot = plot, width = 9, height = 9)
      # Load an image
      image <- image_read(title_plot)
      # Resize it to even dimensions
      image_resized <- image_resize(image, "2832x2832")
      image_write(image_resized, paste0("resized-", title_plot))
      
      Sys.sleep(1)
    }
    
  }
  
}
