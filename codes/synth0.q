#!/bin/bash
#SBATCH -o synth.out
#SBATCH -e synth.err
#SBATCH -c 4
module load GCC/7.4.0
R CMD BATCH --vanilla cluster_0readsynth.R
