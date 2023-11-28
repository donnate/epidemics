#!/bin/bash

# Define the values for the variables
power_values="0.2 1.2 3"
beta_values="0.9 0.5 0.1"
gamma_values="0.1 0.01"
alpha_values="0 0.001 0.01 0.05 0.1"

# Loop over the combinations and launch the sbatch command
for power in $power_values; do
      for beta in $beta_values; do
        for gamma in $gamma_values; do
          for alpha_fp in $alpha_values; do 
            sbatch experiments/simulation_pa.sh 1000 "$beta" "$gamma" "$power" "$alpha_fp"
          done
        done
      done
    done
  done
done
