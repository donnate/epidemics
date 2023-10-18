seed <-  1
result_file <- "debug.csv"
N <- 1000   # Number of nodes
beta_epid <- 0.9 # Infection rate
gamma_epid <-  0.1 # Recovery rate
nb_init <-  1 # Nb of initial patients
power_pa <- 1.2 # parameter of the PA graph
proba_er <- 0.01
heterogeneity_rates <- "none" # are the rates homogeneous?
steps <- 10
p_norm <- "1"
if (p_norm != "inf"){
  p_norm <- ceiling(as.numeric(p_norm))
}
do_plot <- TRUE
p_sw = 0.1
nei = 3
nb_blocks <-  6
proba_between <- 0.01
proba_within <- 0.1
dc_heterogeneity = 1
diffuse = 10
steps = 30




