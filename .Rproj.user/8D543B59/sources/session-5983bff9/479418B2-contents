library(tidyverse)
library(igraph)

# Parameters
n <- 2000 # Number of nodes
p <- 0.01 # Connection probability
nb_communities <- 3
theta_com <- rep(0.05, nb_communities)
Theta <- diag(theta_com)
Theta[1,2] <-0.001
Theta[1,3] <-0.001
Theta[2,3] <-0.005
Theta = 0.5 * ( Theta + t(Theta))
beta <- 0.55 # Infection rate
gamma <- 0.5 # Recovery rate
initial_infected_fraction <- 0.005 # Initial infected fraction
time_steps <- 200


# Create random graph
library(Matrix)
g <- make_full_graph(n)
Z = c( rep(1, ceiling(n/3)), rep(2, ceiling(n/3)), rep(3, n-2 * ceiling(n/3)))
Gamma <-matrix(0, nrow= n*(n-1)/2, ncol=n)
e = 1
edges <- matrix(0, nrow= (n-1)*n/2, ncol = 3)
for (i in 1:(n-1)){
  for (j in (i+1):n){
    Gamma[e, i] = Theta[Z[i], Z[j]]
    Gamma[e, j] = -Theta[Z[i], Z[j]]
    edges[e, 1] = i
    edges[e, 2] = j
    edges[e, 3] =  Theta[Z[i], Z[j]]
    e = e+1
  }
}

# Convert to graph
g <- graph_from_edgelist(as.matrix(edges[, 1:2]), directed = FALSE)
E(g)$weight <- edges[,3]
edge_widths <- E(g)$weight
# Plot the graph with edge labels showing the weights and edge widths based on weights
plot(g, edge.label = E(g)$weight, 
     edge.width = edge_widths)

### Create communities
### Stochastic Block Model, Graphon, ERGM, Latent Variable Model
### Missing at random
layout <- layout_with_fr(g)
plot(g,layout = layout,vertex.size = 4, edge.arrow.size = 0, vertex.label=NA)
# Scale edge widths based on weights

# Initialize state vector (0 for susceptible, 1 for infected)
state <- matrix(0, time_steps + 1  ,n)
prob <- matrix(0, time_steps + 1  ,n)
n_init <- 1 #round(n * initial_infected_fraction)
R0 <- beta/gamma
print(sprintf("R0 is %f", R0))
initial_infected <- sample(1:n, size = n_init)
state[1, initial_infected] <- 1
prob[1, initial_infected] <- 1
beta_v = rep(beta, n) 
gamma_v = rep(gamma, n) 


# Simulate SIS dynamics
t=2
while(t < (time_steps+1) & sum(state[t-1,]>0 )) {
  # Iterate through each node
  for(i in 1:n) {
    if(state[t-1, i] == 1) { # If infected
      # Check recovery
      state[t, i] <- 1 - rbinom(1, 1, gamma)
    }else{
      # Check infection spread to neighbors
      neighbors <- neighbors(g, i)
      infection_force = sum(state[t-1, neighbors] * beta_v[neighbors] * W[i, neighbors])
      # Update Ground Truth
      prob[t, i] = min(max(0, prob[t-1, i] + (1- prob[t-1, i]) * min(1, infection_force) - prob[t-1, i] * gamma_v[i]), 1)
      state[t, i] = rbinom(1, 1, prob[t, i])
    }
  }
  
  # Update state
  cat("Time step", t-1, "- Number of infected:", sum(state[t,]), "\n")
  t = t+1
}








