seed <-  1
result_file <- "test_dcsbm.csv"
n <- 1000   # Number of nodes
beta_epid <-  0.9 # Infection rate
gamma_epid <-  0.1  # Recovery rate
nb_init <-  1 # Nb of initial patients
dc_heterogeneity <- 3 # parameter of the DC-SBM graph
heterogeneity_rates <- "none" # are the rates homogeneous?
steps <- 1
p_norm <- 1
proba_between <- 0.1
proba_within <- 0.01
nb_blocks  <- 6
