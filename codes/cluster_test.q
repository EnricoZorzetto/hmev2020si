#!/bin/bash
#SBATCH -o stats.out
#SBATCH -e stats.err
#SBATCH -c 4
#SBATCH --mem=8G
module load GCC/7.4.0
R CMD BATCH --vanilla '--args kfold_cv=TRUE dataset="G"' cluster_test.R
