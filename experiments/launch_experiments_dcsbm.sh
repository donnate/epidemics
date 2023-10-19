#!/bin/bash

# Define the values for the variables
dcsbm_values="0.5 1 3"
diffuse_values="1 10 30"
beta_values="0.9 0.5 0.1 0.05 0.01"
algorithms="y true_p"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
  for dcsbm_value in $dcsbm_values; do
    for diffuse in $diffuse_values; do
      for beta in $beta_values; do
        sbatch experiments/simulation_dcsbm.sh 1000 "$beta" 0.01 "$proba" 1 none 30 "$diffuse" denoise "$algorithm" 0.005 0.0005 3
      done
    done
  done
done
