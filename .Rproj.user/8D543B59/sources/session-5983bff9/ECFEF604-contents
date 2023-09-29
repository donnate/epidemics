

simulate_epidemic_one_step <- function(W, Y_init,
                                       beta = 0.1, gamma = 0.1){
  n_nodes = ncol(W)
  Y = rep(0, n_nodes)
  prob = rep(0, n_nodes)
  beta_v = rep(beta, n_nodes)
  gamma_v = rep(gamma, n_nodes)
  
  #### Generate ground truth:
  D = diag(nrow=n_nodes) + diag(1-Y_init) %*% diag(beta_v) %*% as.matrix(W) - diag(gamma_v)
  true_p = D %*% Y_init
  Y_observed = sapply(true_p, function(x){rbinom(1,1, max(min(x,1),0))})
  return(list(true_p=true_p,
              Y_observed = Y_observed))
}
