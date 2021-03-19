#!/bin/bash
#SBATCH -o stats0.out
#SBATCH -e stats0.err
#SBATCH -c 4
module load R/4.0.3-rhel8
R CMD BATCH --vanilla '--args kfold_cv=TRUE dataset="G"' cluster_0readstats.R
