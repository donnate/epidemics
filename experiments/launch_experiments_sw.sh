#!/bin/bash

# Define the values for the variables
proba_values="0.5 1 3"
diffuse_values="1 15"
beta_values="0.9 0.1 0.05 0.01"
algorithms="true_p"
gamma_values="0.1 0.01"
alpha_values="0.001 0.005 0.01 0.05"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
  for proba in $proba_values; do
    for diffuse in $diffuse_values; do
      for beta in $beta_values; do
        for gamma in $gamma_values; do
          for alpha_fp in $alpha_values; do 
             sbatch experiments/simulation_sw.sh 1000 "$beta" "$gamma" 1 "$proba" none 30 "$diffuse" "$algorithm" "$alpha_fp"
          done
        done
      done
    done
  done
done
