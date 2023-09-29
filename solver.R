
library(CVXR)
library(roxygen2)
library(pracma)
library(roperators)
library(gear)

CVX_solver <- function(Y_observed, Gamma, lambda){
  n_nodes = length(Y_observed)
  
  p <- Variable(n_nodes)
  constraints <- list(p >= 0, p <= 1)
  
  # Define the quadratic loss
  loss <- sum((Y_observed - p)^2) / n_nodes
  # Define the L-1 norm term
  l1_norm <- cvxr_norm(Gamma %*% p, p = 1)
  # Define the objective
  objective <- Minimize(loss+ lambda * l1_norm)
  # Formulate the problem
  problem <- Problem(objective, constraints)
  # Solve the problem
  result_problem <- solve(problem)
  # Get the optimal value of p
  p_opt <- result_problem$getValue(p)  
  return(p_opt)
}




cgd_solver <- function(Y_observed, X, Gamma, lambda,
                       eps = 1e-4, max_it = 10000){
  m = dim(Gamma)[1]
  n_nodes = length(Y_observed)
  if (is.null(X)==FALSE){
    X_pinv = pinv(X)
    y_v = X %*% (X_pinv %*% Y_observed)
    D_v = Gamma %*% X_pinv
  }else{
    y_v = Y_observed
    D_v = as(Gamma, "sparseMatrix")
  }

  
  Q = D_v %*% t(D_v)
  b = D_v %*% y_v
  
  u = matrix(0, m, 1)
  n_iter = 0
  prev_u = 0
  
  while(TRUE){
    #print(n_iter)
    n_iter %+=% 1
    if(n_iter > max_it){
      print("Iterations exceed max_it")
      if (is.null(X)){
        return((y_v - t(D_v) %*% u))
      }else{
        return(X_pinv %*% (y_v - t(D_v) %*% u))
      }
      
    }
    
    #diagQ = diag(Q)
    #index = which(diagQ < 1e-5)
    #diagQ[index] = 1e-5
    #t = Diagonal(x=1/diagQ) %*% (b - (Q %*% u -Diagonal(x=diag(Q))%*% u))
    #u = sign(t)* sapply(t, function(x){min(abs(x),  lambda)})
    for (i in 1:m){
      print(i)
      t = 1/Q[i,i] * (b[i] - dot(Q[i,][-c(i)], u[-c(i)]))
      u[i] = sign(t)*min(abs(t), lambda)
    }
    #print(u[1:50])
    
    
    print(paste0("niter: ", n_iter, " diff= ", norm(u - prev_u, '2') ))
    if (  norm(u - prev_u, '2') <= eps){
      break
    }
    
    prev_u <- u
  }
  
  if (is.null(X)==FALSE){
    beta = X_pinv %*% (y_v - t(D_v) %*% u)
  }else{
    beta = (y_v - t(D_v) %*% u) 
  }
  return (beta)
}
