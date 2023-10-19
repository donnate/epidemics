#!/bin/bash

# Define the values for the variables
power_values="0.2 1.2 3"
diffuse_values="1 5 10 20"
beta_values="0.9 0.5 0.1 0.05 0.01"
algorithms="y true_p"

# Loop over the combinations and launch the sbatch command
#for algorithm in $algorithms; do
  for power in $power_values; do
    for diffuse in $diffuse_values; do
      for beta in $beta_values; do
        #sbatch experiments/simulation_pa.sh 1000 "$beta" 0.01 1 "$power" none 30 "$diffuse" "$algorithm"
        sbatch experiments/simulation_pa.sh 1000 "$beta" 0.01 1 "$power" none 30 "$diffuse" true_p
      done
    done
  done
#done
