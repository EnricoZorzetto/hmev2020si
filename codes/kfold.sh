#!/bin/bash
echo 'Running kfold cross validation using observed data'
FILE1=../output/output_data/numberofstats_cv.txt
FILE2=../output/output_data/numberofjobs_cv.txt
ns=$(cat "../output/output_data/numberofstats_cv.txt")
echo $ns
nj=$(cat "../output/output_data/numberofjobs_cv.txt")
echo $nj
FILE3=../output/output_data/list_stats_"$ns"_cv.csv
echo "$FILE1"
echo "$FILE2"
echo "$FILE3"
line=$(head -1 $FILE3)
echo $line
if [[ -f "$FILE1" && -f "$FILE2" && -f "$FILE3" ]]; then
    echo "list of stations exists already"
else
    echo "reading the list of stations"
    FIRST=$(sbatch -o con1.out -e con1.err --wait --parsable kfold0.q)
    sleep 2s
    echo $FIRST
fi
n=$(cat "../output/output_data/numberofjobs_cv.txt")
echo "Number of jobs [stations] = $n"
SECOND=$(sbatch -o kfold.out -e kfold.err --array 1-$n --parsable kfold.q)
echo $SECOND
exit 0

