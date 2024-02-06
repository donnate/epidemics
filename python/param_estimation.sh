#!/bin/bash
#SBATCH --job-name=array
#SBATCH --output=experiments/logs/param_%A_%a.out
#SBATCH --error=experiments/logs/param_%A_%a.err
#SBATCH --array=1-100
#SBATCH --time=4:00:00
#SBATCH --partition=caslake
#SBATCH --mem=10G
#SBATCH --account=pi-cdonnat

# Print the task id.
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "My SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
# Add lines here to run your computations
job_id=$SLURM_ARRAY_JOB_ID
result_file="${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
echo "result file is ${result_file}"
cd $SCRATCH/$USER/epidemics_2/epidemics/python

module load gsl
module load gcc
module load python/anaconda-2021.05
source activate epidemics

python3 param_estimation.py --namefile $result_file --seed $SLURM_ARRAY_TASK_ID --n_nodes 1000 --beta $1 --gamma $2 --graph_type knn --p 0 --m $3 --alpha_fp 0 --steps $4