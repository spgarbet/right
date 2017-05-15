#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=30G
#SBATCH -t 1-0:00:00
#SBATCH -o long_all_%A_%a.out
#SBATCH -e long_all_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=zilu.zhou@vanderbilt.edu
#SBATCH --array=0-9

setpkgs -a R_3.3.2-gcc_4.6.1
R --version

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

Rscript all_long_1.R $SLURM_ARRAY_TASK_ID

