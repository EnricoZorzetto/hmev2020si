#!/bin/bash
#SBATCH -o stats.out
#SBATCH -e stats.err
#SBATCH -c 1
#SBATCH --mem=8G
module load GCC/7.4.0
R CMD BATCH --vanilla '--args kfold_cv=FALSE dataset="G"' cluster_stats.R
