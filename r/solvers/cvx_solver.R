
library(CVXR)
library(roxygen2)
library(pracma)
library(roperators)
library(gear)

cvx_solver <- function(y_observed, Gamma, lambda, p_norm = 1){
  n_nodes <- length(y_observed)
  #print(n_edges)
  #print(lambda)
  subject_0 = which(y_observed!=0)
  print(subject_0)
  p <- Variable(n_nodes)
  constraints <- list(p >= 0, p <= 1)
  # Define the quadratic loss
  loss <- sum((y_observed - p)^2) / n_nodes
  # Define the L-1 norm term
  l1_norm <- cvxr_norm(Gamma %*% p, p = p_norm)
  # Define the objective
  objective <- Minimize(loss + lambda * l1_norm)
  # Formulate the problem
  problem <- Problem(objective, constraints)
  # Solve the problem
  result_problem <- solve(problem)
  # Get the optimal value of p
  p_opt <- result_problem$getValue(p)
  return(p_opt)
}
