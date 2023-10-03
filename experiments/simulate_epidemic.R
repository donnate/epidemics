

simulate_epidemic <- function(W, y_init,
                              beta_v,
                              gamma_v,
                              steps = 1) {
  # W : weighted graph adjacency matrix
  # y_init : initial observations
  # beta :  infection probability
  # gamma : recovery probability
  n_nodes <- ncol(W)

  #### Generate ground truth:
  y <- y_init
  for (step in 1:steps){
    prop <- propagate_one_step(W, y, beta_v, gamma_v)
    y <- prop$y
    true_p <- prop$true_p
  }
  return(list(true_p = true_p,
              y_observed = y,
              beta_v = beta_v,
              gamma_v = gamma_v))
}

propagate_one_step <- function(W, y,
                               beta_v, gamma_v) {
  # W : weighted graph adjacency matrix
  # y_init : initial observations
  # beta :  infection probability
  # gamma : recovery probability
  n_nodes <- ncol(W)
  D <- diag(nrow=n_nodes) + diag(1 - y) %*% diag(beta_v) %*% as.matrix(W) - diag(gamma_v)
  true_p <- D %*% y
  y <- sapply(true_p, function(x) { rbinom(1, 1, max(min(x, 1), 0)) })
  return(list(true_p = true_p, y = y))
}