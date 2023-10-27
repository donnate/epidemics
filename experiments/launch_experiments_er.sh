#!/bin/bash

# Define the values for the variables
proba_values="0.001 0.01 0.005"
diffuse_values="1 10"
beta_values="0.9 0.5 0.1 0.05"
gamma_values="0.1 0.01"
algorithms="true_p"
alpha_values="0.001 0.005 0.01 0.05"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
    for proba in $proba_values; do
      for diffuse in $diffuse_values; do
        for beta in $beta_values; do
<<<<<<< HEAD
          sbatch experiments/simulation_er.sh 1000 "$beta" 0.1 1 "$proba" none 20 "$diffuse" "$algorithm"
          # echo "sbatch experiments/simulation_er.sh 1000 $beta 0.1 1 $proba none 30 1 $diffuse denoise"  
=======
          for gamma in $gamma_values; do
            for alpha_fp in $alpha_values; do 
              sbatch experiments/simulation_er.sh 1000 "$beta" "$gamma" 1 "$proba" none 20 "$diffuse" denoise "$alpha_fp"
            done
          # echo "sbatch experiments/simulation_er.sh 1000 $beta 0.1 1 $proba none 30 1 $diffuse denoise" 
          done 
>>>>>>> 04a00f1fdb69010a167d0b6ca3dbfd88924c328c
        done
      done
    done
done
