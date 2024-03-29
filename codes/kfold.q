#!/bin/bash
#SBATCH -o stats.out
#SBATCH -e stats.err
#SBATCH -c 4
#SBATCH --mem=8G
module load R/4.0.3-rhel8
R CMD BATCH --vanilla '--args kfold_cv=TRUE dataset="G"' cluster_stats.R
