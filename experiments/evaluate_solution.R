source("experiments/simulate_epidemic.R")

evaluate_solution <- function(y_observed, p_hat, true_prob, Gamma){
  return(data.frame(
    "risk_observed" = mean((y_observed - p_hat)^2),
    "oracle" = mean((y_observed -  true_prob)^2),
    "l2_error" = mean((true_prob - p_hat)^2),
    "l1_error" = mean(abs(true_prob - p_hat)),
    "Gamma_l1_error" = mean(abs(Gamma %*% (true_prob - p_hat))),
    "Gamma_l2_error" = mean(((Gamma %*% (true_prob - p_hat)))^2),
    "Gamma_l1_error_benchmark" = mean(abs(Gamma %*% (true_prob - y_observed))),
    "Gamma_l2_error_benchmark" = mean(((Gamma %*% (true_prob - y_observed)))^2)
  )
  )
}


propagate_solution <- function(W, p_hat, beta_v, gamma_v, nb_steps) {
  p_current <- p_hat
  list_p <- vector("list", nb_steps)
  for (it in 1:nb_steps){
    prop_results <- propagate_one_step(W, as.numeric(p_current), beta_v, gamma_v)
    p_current <- sapply(prop_results$true_p, function(x){min(max(x,0), 1)})
    list_p[[it]] <- p_current
  }
  return(list_p)
}
