#### Generate a graph
library(igraph)
library(Matrix)
library(tidyverse)
theme_set(theme_bw(base_size = 14))


generate_graph <- function(n_nodes, 
                           n_infected = 20,
                           weight_type = "DC-SBM",
                           n_communities=4,
                           theta_between = 0.001,
                           theta_within = 0.01,
                           do.plot=FALSE,
                           threshold=1e-5){
  
  if (weight_type == "DC-SBM" ||weight_type == "SBM"  ){
    ### Generate node label
    Z = sapply(1:n_nodes, function(x){(x-1) %/% floor(n_nodes/n_communities)})
    theta = ifelse(weight_type == "DC-SBM", rexp(n_nodes, rate=1), rep(1, n_nodes))
    ### Add constraint
    for (z in unique(Z)){
      index = which(Z == z)
      theta[index] = length(index) * theta[index]/sum(theta[index])
    }
    ### Assign edge weights and define edge incidence matrix
    #sparseGamma <- sparseMatrix(i = integer(0), j = integer(0), dims = c(n_nodes * (n_nodes-1)/2, 2 + n_nodes))

    L = outer(theta, theta)
    P = outer(Z, Z, "==") * theta_within + outer(Z, Z, "!=") * theta_between
    W = L * P
    diag(W) = 0
    W[which(W < threshold)] =0
    
    
    ###
    if (do.plot){
      W_df = as.data.frame(W)
      colnames(W_df) = 1:n_nodes
      W_df["id"] = 1:n_nodes
      ggplot(pivot_longer(W_df, cols =-c("id")))+
        geom_tile(aes(x=id, y=name, fill=value))+
        scale_fill_viridis_c()
    }
    ###
    edges = which(W >0, arr.ind = TRUE)
    weights = W[edges]
    Gamma = matrix(0, nrow=nrow(edges), ncol=n_nodes)
    for(i in 1:length(weights)) {
      Gamma[i, edges[i, 1]] <- -weights[i]
      Gamma[i, edges[i, 2]] <- weights[i]
    }
  }else{
    #### generate random graph with random weights
    Z = rep(1, n_nodes)
    theta = weights
    g <- sample_pa(n_nodes, power=1.2, directed = FALSE)
    #layout <- layout_with_fr(g)
    #plot(g,layout = layout,vertex.size = 4, edge.arrow.size = 0, vertex.label=NA)
    edges = data.frame(as_edgelist(g))%>% filter(X1 < X2)
    weights = runif(n=nrow(edges))
    Gamma = matrix(0, nrow(edges), n_nodes)
    Gamma[,edges$X1] = weights
    Gamma[,edges$X2] = - weights
    Gamma = rbind(Gamma, -Gamma)
    W = as_adjacency_matrix(g)
    it = 1
    for (e in edge_list){
      W[e[1], e[2]] = weights[it]
      W[e[2], e[1]] = weights[it]
      it = it + 1
    }

  }

  return(list(Gamma=Gamma, 
              edges=edges,
              weights= weights,
              W=W,
              Z=Z,
              theta=theta))
}




