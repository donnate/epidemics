#!/bin/bash

# Define the values for the variables
alpha_values="0 0.001 0.005 0.01 0.05 0.1"

# Loop over the combinations and launch the sbatch command
for alpha_fp in $alpha_values; do 
    sbatch experiment_gen_graph.sh $1 $2 "$alpha_fp"
done
