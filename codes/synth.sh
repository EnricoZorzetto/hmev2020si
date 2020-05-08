#!/bin/bash
echo 'Running hbev analysis using synthetic data'
# FILE1=../output/output_data/synth_number_of_jobs.txt
# n=10
# echo "Number of jobs = $n"
FIRST=$(sbatch -o synth0.out -e synth0.err --wait --parsable synth0.q)
sleep 2s
n=$(cat "../output/output_data/synth_number_of_jobs.txt")
echo "Number of simulation jobs = $n"
SECOND=$(sbatch -o synth.out -e synth.err --array 1-$n --parsable synth.q)
echo $SECOND
exit 0

