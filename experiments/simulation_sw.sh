#!/bin/bash
#SBATCH --job-name=array
#SBATCH --output=experiments/logs/array_pa_%A_%a.out
#SBATCH --error=experiments/logs/array_pa_%A_%a.err
#SBATCH --array=1-10
#SBATCH --time=6:00:00
#SBATCH --partition=caslake
#SBATCH --ntasks=5
#SBATCH --mem=20G
#SBATCH --account=pi-cdonnat

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
module load gsl
module load gcc
module load R/4.2.0

result_file="res_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
echo "result file is ${result_file}"
cd $SCRATCH/$USER/epidemic_modelling/
Rscript experiments/simulation_sw.R $SLURM_ARRAY_TASK_ID $result_file $1 $2 $3 $4 $5 $6 $7 $8
#$1: n (population size)
#$2: beta (infection rate)
#$3: gamma (recovery probability)
#$4: nb of ``patient zero''s
#$5: parameter of the SW graph (rewiring probability)
#$6: heterogeneity of the rates
#$7: nb of steps for the epidemic
#$8: norm for the penalty (inf or 1)
#$9: neighbourhood