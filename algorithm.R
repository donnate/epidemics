library(CVXR)


edge_list = data.frame(as_edgelist(g))%>% filter(X1 < X2)
Gamma = matrix(0, nrow(edge_list), n)
Gamma[,edge_list$X1] = 1
Gamma[,edge_list$X2] = - 1
# Define variable
t = 3
Y = state[t,]
p <- Variable(n)
lambda=1

# Define constraints
constraints <- list(p >= 0, p <= 1)

# Define the quadratic loss
loss <- sum((Y - p)^2) / n

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
sum((prob[t,] - p_opt)^2)
sum((Y- prob[t,])^2)
sum((Y- p_opt)^2) # Residual

t=3
colors <- ifelse(state[t,] == 1, "red", "blue")

# Plot the graph with node colors based on infection status
plot(g, vertex.color = colors, main = "Node Infection Status")
#layout <- layout_with_fr(g)
layout <- layout_with_fr(g)

vertex_sizes <- (p_opt - min(p_opt)) / diff(range(p_opt)) * 10 
plot(g,layout = layout,vertex.size = vertex_sizes, edge.arrow.size = 0, vertex.label=NA,
     vertex.color = colors)


library(RColorBrewer)
scaled_values <- scale(p_opt, center = min(p_opt), scale = diff(range(p_opt)))
scaled_values <- as.integer(cut(scaled_values, breaks = length(brewer.pal(11, "Spectral"))))

# Map the scaled values to the "Spectral" palette
colors <- brewer.pal(11, "Spectral")[scaled_values]
plot(g, layout = layout,vertex.size = 4, edge.arrow.size = 0, vertex.label=NA,
     vertex.color = colors)

# Plot the graph with node colors based on infection status
plot(g, vertex.color = colors, main = "Node Infection Status")
p_true = prob[t,]
vertex_sizes <- (p_true - min(p_true)) / diff(range(p_true)) * 10 
layout <- layout_with_fr(g)
plot(g,layout = layout,vertex.size = 4, edge.arrow.size = 0, vertex.label=NA,
     vertex.color = colors)

