

get_edge_incidence <- function(g, graph = "SBM", Theta=NULL,
                               Z = NULL){
  n_nodes = vcount(g)
  edges = data.frame(as_edgelist(g)) %>% 
    filter(X1 < X2)
  Gamma = matrix(0, nrow(edges), n_nodes)
  if (graph  != "SBM"){
    weights = runif(n=nrow(edges))
  }else{
    weights = sapply(1:nrow(edges), function(i){Theta[Z[edges$X1[i]], Z[edges$X2[i]]]})
  }
  edges["weights"]= weights
  g2 <- graph_from_data_frame(edges, directed = FALSE)  # Set directed to TRUE if you have a directed graph
  E(g2)$weight = edges$weights
  adj_matrix <- as_adjacency_matrix(g2, attr = "weight", sparse = FALSE)
  Gamma[,edges$X1] = weights
  Gamma[,edges$X2] = - weights
  return(list(Gamma=Gamma,
              W = adj_matrix))
}
