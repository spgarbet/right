#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=8G
#SBATCH -t 0-2:59
#SBATCH -o sing_c_%A_%a.out
#SBATCH -e sing_c_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=zilu.zhou@vanderbilt.edu
#SBATCH --array=0-9

setpkgs -a  R_3.2.0_gcc
R --vanilla --slave < <(cat cs_${SLURM_ARRAY_TASK_ID}.R)
