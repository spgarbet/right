#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=25G
#SBATCH -t 0-5:59
#SBATCH -o all50_long_%A_%a.out
#SBATCH -e all50_long_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=zilu.zhou@vanderbilt.edu

setpkgs -a  R_3.2.0_gcc
R --vanilla --slave < all50_long_1.R
