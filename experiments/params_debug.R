seed <-  1
result_file <- "debug.csv"
N <- 1000   # Number of nodes
beta_epid <- 0.9 # Infection rate
gamma_epid <-  0.01 # Recovery rate
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
p_sw = 0.5
nei = 1
nb_blocks <-  3
proba_between <- 0.0005
proba_within <- 0.005
dc_heterogeneity = 1
diffuse = 100
steps = 30

propagation  = "true_p"


