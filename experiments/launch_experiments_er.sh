#!/bin/bash

# Define the values for the variables
proba_values="0.001 0.005 0.01 0.1"
diffuse_values="1 10 20"
beta_values="0.9 0.5 0.1 0.05"
algorithms="y true_p"

# Loop over the combinations and launch the sbatch command
for algorithm in $algorithms; do
    for proba in $proba_values; do
      for diffuse in $diffuse_values; do
        for beta in $beta_values; do
          sbatch experiments/simulation_er.sh 1000 "$beta" 0.1 1 "$proba" none 20 "$diffuse" denoise
          # echo "sbatch experiments/simulation_er.sh 1000 $beta 0.1 1 $proba none 30 1 $diffuse denoise"  
        done
      done
    done
done
