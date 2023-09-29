setwd("~/Documents/epidemic_modelling/")
library(CVXR)
library(tidyverse)
theme_set(theme_bw(base_size = 14))
source("generate_graph.R")


n_nodes = 100
n_infected = 5
weight_type = "random"
n_communities = 4
theta_between = 0.001
theta_within = 0.01
beta = runif(n_nodes, min=0.05, max=0.3)
gamma = runif(n_nodes, min=0.3, max=0.6)

graph <- generate_graph(n_nodes, n_infected = n_infected,
                        weight_type = weight_type,
                        n_communities = n_communities,
                        theta_between = theta_between,
                        theta_within = theta_within,
                        threshold =  1 * 1e-2,
                        do.plot = FAlSE)
dim(graph$Gamma)

#### Select a bunch of observations that are contaminted
Y_init = rep(0, n_nodes)
infected = sample(1:n_nodes, n_infected)
Y_init[infected] = 1 
prob_init = Y_init

Y = rep(0, n_nodes)
prob = rep(0, n_nodes)


beta_v = beta
gamma_v = gamma

#### Generate ground truth:
D = diag(nrow=n_nodes) + diag(1-Y_init) %*% diag(beta) %*% as.matrix(graph$W) - diag(gamma)
true_p = D %*% Y_init
Y_observed = sapply(true_p, function(x){rbinom(1,1, max(min(x,1),0))})

#### Solve the problem using trend filtering

result = c()
for (lambda in exp(seq(from=-5, -1, 0.5) * log(10))){
  print(lambda)
  
  # Define constraints
  p <- Variable(n_nodes)
  p_opt = CVX_solver(Y_observed, graph$Gamma, lambda)
  p_opt <- cgd_solver(Y_observed, X=NULL, Gamma=graph$Gamma, 
                         lambda=0.0001, eps = 1e-2, max_it = 100)
  out = dualpathFusedL1(Y_observed, graph$Gamma, graph$Gamma,0.0001,verbose=TRUE)
  #### plot it on a graph
  result = rbind(result,
                 data.frame(
                   "lambda" = lambda,
                   "risk" = mean((Y_observed - p_opt)^2),
                   "oracle" = mean((Y_observed - prob)^2),
                   "l2_error" = mean((prob - p_opt)^2),
                   "l1_error" = mean(abs(prob - p_opt))
                 )
  )
  
  
}


ggplot(pivot_longer(result, cols=-c("lambda")),
       aes(x=lambda, y=value, colour=name))+
  geom_line(size=2) +
  geom_hline(aes(yintercept=sum(Y)/n_nodes, colour="Constant solution")) + 
  scale_x_log10()
theme_bw()




for (lambda in exp(seq(from=-2, 2, 0.5) * log(10))){
  print(lambda)
  
  # Define constraints
  p <- Variable(n_nodes)
  constraints <- list(p >= 0, p <= 1)
  
  # Define the quadratic loss
  loss <- sum((Y_observed - p)^2) / n_nodes
  
  # Define the L-infinity norm term
  l1_norm <- cvxr_norm(graph$Gamma %*% p, p = 1)
  
  # Define the objective
  objective <- Minimize(loss+ lambda * l1_norm)
  
  # Formulate the problem
  problem <- Problem(objective, constraints)
  
  # Solve the problem
  result_problem <- solve(problem)
  
  # Get the optimal value of p
  p_opt <- result_problem$getValue(p)
  #p_opt[which(p_opt<1e-7)] = 0
  
  #### plot it on a graph
  result = rbind(result,
                 data.frame(
                   "lambda" = lambda,
                   "risk" = mean((Y_observed - p_opt)^2),
                   "oracle" = mean((Y_observed - prob)^2),
                   "l2_error" = mean((prob - p_opt)^2),
                   "l1_error" = mean(abs(prob - p_opt))
                 )
  )
  
  
}


ggplot(pivot_longer(result, cols=-c("lambda")),
       aes(x=lambda, y=value, colour=name))+
  geom_line(size=3) +
  geom_hline(aes(yintercept=sum(Y)/n_nodes, colour="Constant solution")) + 
  scale_x_log10()
theme_bw()





Y = rep(0, n_nodes)
infected = sample(1:n_nodes, n_infected)
Y[infected] = 1 
#### Solve the problem using trend fltering
p <- Variable(n_nodes)
lambda=10

# Define constraints
constraints <- list(p >= 0, p <= 1)

# Define the quadratic loss
loss <- sum((Y - p)^2) / n_nodes

# Define the L-infinity norm term
linf_norm <- cvxr_norm(Gamma %*% p, "inf")

# Define the objective
objective <- Minimize(loss + lambda * linf_norm)

# Formulate the problem
problem <- Problem(objective, constraints)

# Solve the problem
result <- solve(problem)

# Get the optimal value of p
p_opt <- result$getValue(p)

#### plot it on a graph
sum((prob - p_opt)^2)
sum((Y- p_opt)^2)
sum((Y- Y_init)^2) # Residual

out = dualpathFusedL1(Y_observed,Gamma,Gamma,gamma=0,verbose=TRUE)
