#!/bin/bash

# Define the values for the variables
power_values="0.2 1.2"
diffuse_values="1 5 10 20"
beta_values="0.9 0.8 0.5 0.1 0.05"

# Loop over the combinations and launch the sbatch command
for power in $power_values; do
  for diffuse in $diffuse_values; do
    for beta in $beta_values; do
      sbatch experiments/simulation_pa.sh 1000 "$beta" 0.1 "$power" 1 none 30 1 "$diffuse" denoise
    done
  done
done