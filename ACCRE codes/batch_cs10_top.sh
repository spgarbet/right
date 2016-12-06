#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=25G
#SBATCH -t 0-3:59
#SBATCH -o sing_c_%j.out
#SBATCH -e sing_c_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=zilu.zhou@vanderbilt.edu

setpkgs -a  R_3.2.0_gcc
R --vanilla --slave < cs_top.R

