

get_edge_incidence <- function(g, beta_v,
                               graph = "SBM", Theta=NULL,
                               Z = NULL, weight = 1){
  n_nodes = vcount(g)
  edges = data.frame(as_edgelist(g)) %>%
    arrange(X1, X2)
  W = matrix(0, nrow = n_nodes, ncol=n_nodes)
  Gamma = matrix(0, nrow(edges), n_nodes)
  if (graph  != "SBM"){
    if (weight == 1) {
      weights = rep(1/n_nodes, nrow(edges))
    } else {
      weights = runif(n=nrow(edges), min = 0, max=1/n_nodes) #dirichlet
    }
  }else{
    weights = sapply(1:nrow(edges), function(i){Theta[Z[edges$X1[i]], Z[edges$X2[i]]]})
    weights = weights/(max(Theta) * n)
  }
  #infection_weight = 
  edges["weights"]= weights
  # Make beta_v into a matrix
  for (e in 1:nrow(edges)){
    W[edges$X1[e], edges$X2[e]] = edges$weights[e] * (beta_v[edges$X2[e]] + beta_v[edges$X1[e]])/2
    W[edges$X2[e], edges$X1[e]] = edges$weights[e] * (beta_v[edges$X2[e]] + beta_v[edges$X1[e]])/2
    Gamma[e,edges$X1[e]] = edges$weights[e] * (beta_v[edges$X2[e]] + beta_v[edges$X1[e]])/2
    Gamma[e,edges$X2[e]] = - edges$weights[e] * (beta_v[edges$X2[e]] + beta_v[edges$X1[e]])/2
  }
  return(list(Gamma=Gamma,
              W = W))
}
