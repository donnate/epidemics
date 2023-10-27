#!/bin/bash

# Define the values for the variables
power_values="0.2 1.2 3"
diffuse_values="1 10 20"
beta_values="0.9 0.5 0.1 0.05 0.01"
algorithms="true_p"
gamma_values="0.1 0.01"
alpha_values="0.001 0.005 0.01 0.05"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
  for power in $power_values; do
    for diffuse in $diffuse_values; do
      for beta in $beta_values; do
<<<<<<< HEAD
        sbatch experiments/simulation_pa.sh 1000 "$beta" 0.01 1 "$power" none 30 "$diffuse" "$algorithm"
        #sbatch experiments/simulation_pa.sh 1000 "$beta" 0.01 1 "$power" none 30 "$diffuse" true_p
=======
        for gamma in $gamma_values; do
          for alpha_fp in $alpha_values; do 
            sbatch experiments/simulation_pa.sh 1000 "$beta" "$gamma" 1 "$power" none 30 "$diffuse" "$algorithm" "$alpha_fp"
          done
        done
>>>>>>> 04a00f1fdb69010a167d0b6ca3dbfd88924c328c
      done
    done
  done
done
