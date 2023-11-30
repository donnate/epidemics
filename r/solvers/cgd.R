cgd_solver <- function(y_observed, X , Gamma,
                       lambda,
                       eps = 1e-4, max_it = 10000) {
  m <- dim(Gamma)[1]
  if (is.null(X)==FALSE) {
    X_pinv <- pinv(X)
    y_v <- X %*% (X_pinv %*% y_observed)
    D_v <- Gamma %*% X_pinv
  }else {
    y_v <- y_observed
    D_v <- as(Gamma, "sparseMatrix")
  }

  Q <- D_v %*% t(D_v)
  b <- D_v %*% y_v
  u <- matrix(0, m, 1)
  n_iter <- 0
  prev_u <- 0
  while(TRUE) {
    #print(n_iter)
    n_iter %+=% 1
    if(n_iter > max_it) {
      print("Iterations exceed max_it")
      if (is.null(X)) {
        return((y_v - t(D_v) %*% u))
      }else {
        return(X_pinv %*% (y_v - t(D_v) %*% u))
      }
    }
    for (i in 1:m) {
      print(i)
      t = 1/Q[i,i] * (b[i] - dot(Q[i,][-c(i)], u[-c(i)]))
      u[i] = sign(t) * min(abs(t), lambda)
    }

    print(paste0("niter: ", n_iter,  " diff= ", norm(u - prev_u,'2' )))
    if ( norm(u - prev_u, '2') <= eps) {
      break
    }
    prev_u <- u
  }
  if (is.null(X) == FALSE) {
    beta <- X_pinv %*% (y_v - t(D_v) %*% u)
  } else {
    beta <- (y_v - t(D_v) %*% u)
  }
  return(beta)
}