#!/bin/bash
#SBATCH --job-name=array
#SBATCH --output=python/experiments/logs/array_er_%A_%a.out
#SBATCH --error=python/experiments/logs/array_er_%A_%a.err
#SBATCH --array=1-1
#SBATCH --time=36:00:00
#SBATCH --partition=caslake
#SBATCH --mem=20G
#SBATCH --account=pi-cdonnat

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
result_file="new_res_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
echo "result file is ${result_file}"
cd $SCRATCH/$USER/epidemic_modelling/python

module load gsl
module load gcc
module load python/anaconda-2021.05
conda activate epidemics

Rscript experiments/experiment_algo.py
#$1: n (population size)
#$2: beta (infection rate)
#$3: gamma (recovery probability)
#$4: parameter of the er graph
#$5: alpha_fp
