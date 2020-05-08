#!/bin/bash
echo 'Running hbev analysis using observed data'
FILE1=../output/output_data/numberofstats_50y.txt
# FILE1=../output/output_data/numberofstats.txt
FILE2=../output/output_data/numberofjobs_50y.txt
# FILE2=../output/output_data/numberofjobs.txt
ns=$(cat "../output/output_data/numberofstats_50y.txt")
# ns=$(cat "../output/output_data/numberofstats.txt")
echo $ns
# nj=$(cat "../output/output_data/numberofjobs.txt")
nj=$(cat "../output/output_data/numberofjobs_50y.txt")
echo $nj
FILE3=../output/output_data/list_stats_50y"$ns".csv
# FILE3=../output/output_data/list_stats_"$ns".csv
echo "$FILE1"
echo "$FILE2"
echo "$FILE3"
line=$(head -1 $FILE3)
echo $line
if [[ -f "$FILE1" && -f "$FILE2" && -f "$FILE3" ]]; then
    echo "list of stations exists already"
else
    echo "reading the list of stations"
    FIRST=$(sbatch -    o con1.out -e con1.err --wait --parsable stats0.q)
    sleep 2s
    echo $FIRST
fi
# n=$(cat "../output/output_data/numberofjobs.txt")
n=$(cat "../output/output_data/numberofjobs_50y.txt")
echo "Number of jobs [st    ations] = $n"
SECOND=$(sbatch -o stats.out -e stats.err --array 1-$n --parsable stats.q)
echo $SECOND
exit 0

