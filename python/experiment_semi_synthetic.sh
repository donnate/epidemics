#!/bin/bash
#SBATCH --job-name=array
#SBATCH --output=experiments/logs/array_semisynth_er_%A_%a.out
#SBATCH --error=experiments/logs/array_semisynth_er_%A_%a.err
#SBATCH --array=1-20
#SBATCH --time=12:00:00
#SBATCH --partition=caslake
#SBATCH --mem=20G
#SBATCH --account=pi-cdonnat

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
result_file="${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
echo "result file is ${result_file}"
cd $SCRATCH/$USER/epidemics/python

module load gsl
module load gcc
module load python/anaconda-2021.05
conda activate epidemics

python3 experiments/experiment_soc.py --namefile $result_file --seed $SLURM_ARRAY_TASK_ID