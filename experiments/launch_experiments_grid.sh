#!/bin/bash

# Define the values for the variables
dcsbm_values="0.5 1 3"
diffuse_values="1 5 10 20 30"
beta_values="0.9 0.8 0.5 0.1 0.05 0.01"
algorithms="y true_p"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
  for diffuse in $diffuse_values; do
    for beta in $beta_values; do
        sbatch experiments/simulation_grid.sh 1000 "$beta" 0.1 "$proba" 1 none 30 1 "$diffuse" denoise
    done
  done
done

