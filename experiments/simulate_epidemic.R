

simulate_epidemic <- function(W, y_init,
                              beta_v,
                              gamma_v,
                              steps = 1, 
                              propagate = "true_p",
                              alpha_fp = alpha_fp) {
  # W : weighted graph adjacency matrix
  # y_init : initial observations
  # beta :  infection probability
  # gamma : recovery probability
  n_nodes <- ncol(W)

  #### Generate ground truth:
  y <- y_init
  true_p <- y_init
  track_state <- matrix(0, n_nodes, steps + 1)
  track_state[, 1] <- y_init
  for (step in 1:steps) {
    if (propagate == "true_p") {
      prop <- propagate_one_step(W, as.numeric(true_p), beta_v, gamma_v)
      true_p <- prop$true_p
    }else {
      prop <- propagate_one_step(W, as.numeric(y), beta_v, gamma_v)
      true_p <- prop$true_p
      y <- sapply(true_p, function(x) { rbinom(1, 1, min(x, 1)) }) # realization of a random event
    }
    track_state[, 1 + step] <- true_p
  }
  y_true <- sapply(true_p, function(x) { rbinom(1, 1, min(x, 1)) }) 
  it_p <- 1
  while ((sum(y_true)  == 0) &&  it_p < 1000) {
    ## resample to make sure someone is infectious (we must see someone infectious)
    y_true <- sapply(true_p, function(x) { rbinom(1, 1, min(x, 1)) })
    it_p <- it_p + 1
  }

  y_false <- sapply(true_p, function(x) { rbinom(1, 1, alpha_fp) })
  y_observed = y_true + y_false
  y_observed[which(y_observed > 1)] = 1
  return(list(true_p = true_p,
              y_observed = y_observed,
              y_true = y_true,
              y_false = y_false,
              beta_v = beta_v,
              gamma_v = gamma_v,
              track_state = track_state))
}

propagate_one_step <- function(W, y,
                               beta_v, gamma_v) {
  # W : weighted graph adjacency matrix
  # y_init : initial observations
  # beta :  infection probability
  # gamma : recovery probability
  n_nodes <- ncol(W)
  D <- diag(nrow=n_nodes)- diag(gamma_v) + diag(1 - y) %*% diag(beta_v) %*% as.matrix(W)  
  true_p <- D %*% y
  true_p <- sapply(true_p, function(x) { max(min(x, 1), 0) })
  ### if y = 1, the first part equals to 1-gamma_v, the second is 0
  ### if y = 0, the first part equals to 0, the second is beta_v * sum_j  W_{ij} * y_j
  y <- sapply(true_p, function(x) { rbinom(1, 1, x) })
  #print(sum(y))
  #print(which(y>0))
  return(list(true_p = true_p, y = y))
}
