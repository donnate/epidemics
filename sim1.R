library(tidyverse)
library(igraph)
source("graph_utils.R")
source("graph_utils.R")
# Parameters
n <- 1000 # Number of nodes
p <- 0.01 # Connection probability
beta <- 0.55 # Infection rate
gamma <- 0.5 # Recovery rate
initial_infected_fraction <- 0.005 # Initial infected fraction
time_steps <- 200


# Create random graph

g <- sample_pa(n, power=1.2, directed = FALSE)
graph_attributes <- get_edge_incidence(g, graph="PA")
### Stochastic Block Model, Graphon, ERGM, Latent Variable Model
### Missing at random
layout <- layout_with_fr(g)
plot(g,layout = layout,vertex.size = 4, edge.arrow.size = 0, vertex.label=NA)
epidemic <- simulate_epidemic_one_step(W, Y_init, beta = 0.1, gamma = 0.1){