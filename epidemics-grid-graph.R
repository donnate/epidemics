library(tidyverse)
library(igraph)

# Parameters
n <- 4000 # Number of nodes 
p <- 0.01 # Connection probability
beta <- 0.55 # Infection rate
gamma <- 0.2 # Recovery rate
initial_infected_fraction <- 0.005 # Initial infected fraction
time_steps <- 200


# Create random graph
library(Matrix)
g <- make_lattice(dimvector  = c(sqrt(n), sqrt(n)))
n <- vcount(g)

### Assign edge weights and define edge incidence matrix
Gamma <- matrix(0, ncol=vcount(g), nrow=ecount(g))

for (e in 1:ecount(g)) {
  verts <- ends(g, E(g)[e])
  w <- ifelse(verts[1] < sqrt(n)/2, 0.5, 0.1) * ifelse(verts[2] < sqrt(n)/2, 0.5, 1)
  Gamma[e, verts[1]] <- w
  Gamma[e, verts[2]] <- -w
}

# Initialize state vector (0 for susceptible, 1 for infected)
state <- matrix(0, time_steps + 1  ,vcount(g))
prob <- matrix(0, time_steps + 1  ,vcount(g))
n_init <- round(n * initial_infected_fraction)
R0 <- beta/gamma
print(sprintf("R0 is %f", R0))
initial_infected <- sample(1:vcount(g), size = n_init)
state[1, initial_infected] <- 1
prob[1, initial_infected] <- 1
beta_v = rep(beta, vcount(g)) 
gamma_v = rep(gamma, vcount(g)) 


# Simulate SIS dynamics
t=2
neighbours_matrix = matrix(0, vcount(g), vcount(g))
for(i in 1:vcount(g)) {
  neighs = as.numeric(neighbors(g, i))
  weights = sapply(neighs, function(x){
    ifelse(x < sqrt(n)/2, 0.5, 0.1) * ifelse(i < sqrt(n)/2, 0.5, 1)
  })
  neighbours_matrix[i, neighs] = weights
}

while(t < (time_steps+1) & sum(state[t-1,]>0 )) {
  # Iterate through each node
  for(i in 1:vcount(g)) {
    #print(i)
    if(state[t-1, i] == 1) { # If infected
      # Check recovery
      state[t, i] <- 1 - rbinom(1, 1, gamma)
    }else{
      # Check infection spread to neighbors
      neighbors = which(neighbours_matrix[i,]>0)
      infection_force = sum(state[t-1, neighbors] * beta_v[neighbors] * neighbours_matrix[i, neighbors])
      # Update Ground Truth
      prob[t, i] = min(max(0, prob[t-1, i] + (1- prob[t-1, i]) * min(1, infection_force) - prob[t-1, i] * gamma_v[i]), 1)
      state[t, i] = rbinom(1, 1, prob[t, i])
    }
  }
  
  # Update state
  cat("Time step", t-1, "- Number of infected:", sum(state[t,]), "\n")
  t = t+1
}

#### Let us visualize this on a network

coordinates = data.frame(layout_on_grid(g))
colnames(coordinates) = c("X", "Y")
df = cbind(coordinates, t(state))
df_long = pivot_longer(df, cols =-c("X", "Y"))
df_long$name = as.numeric(df_long$name)
ggplot(df_long %>% filter(name>30, name <50), aes(x=X, y=Y, fill=value))+
  geom_tile()+
  facet_wrap(.~name)


edges_ids <- get.edge.ids(g, pairs = matrix(c(1,2, 2,3), ncol=2, byrow=TRUE))








