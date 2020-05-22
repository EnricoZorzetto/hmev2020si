
## Codes for replicating the analysis described in the manuscript: "Bayesian non-asymptotic extreme value models for environmental data"
 
### Enrico Zorzetto, May 22, 2020



Together with the manuscript, we provide two code repositories:

* The _"hmevr"_ folder: contains the R package which implements the extreme value models described in the manuscript. The R package includes vignettes featuring a tutorial and some of the analysis shown in the paper. This can be installed from source, or alternatively from the Github repository _'EnricoZorzetto/hmevr'_.

* The _"hmev2020si"_ folder: contains the codes to download and clean the data,
  and replicates the full simulation study and data analysis. The structure of this folder is as follows:
  + _"hmev2020si/codes"_: contains all the scripts used for the analysis
  + _"hmev2020si/data"_: where downloaded and cleaned data are stored
  + _"hmev2020si/output"_: where output data and figures are stored
  
* Data can be downloaded and processed by running the script _"read_ushcn_daily.R"_
  
  
*  Note that part of the analysis involve a single observed or simulated dataset. These can be easily run from a personal computer, and include the following scripts:
    + _example_nycp_station.R:_ Analyzes the New York time series
    + _example_synth_data.R:_ Analyzes a sample simulated time series
    + _example_synth_data_4specs.R:_ Analyzes 4 different simulated time series
  
* The analysis of the entire dataset and the full simulation study was performed on a computer cluster. The bash scripts used for the analysis are the following:

  + _synth.sh_ This script launches the simulation study
  + _kfold.sh_ This script launches the full data analysis of the USHCN dataset, following the the in-sample vs out-of-sample validation analysis described in the paper
  + _stats.sh_ This script fit EV models to all the stations in the dataset, without out-of-sample validation (This was not used in the paper).
  
###  Description of the analysis

To reproduce the entire analysis, run the two scripts:\
  $ _bash synth.sh_\
  $ _bash kfold.sh_\
  
The station analysis is performed by _kfold.sh_, which launches two sbatch jobs. First, a file _kfold0.q_ runs the script _cluster0_readstats.R_ which determines how many jobs are needed, and a second sbatch file _kfold.q_ launches an array of jobs in parallel. Each of them runs the script _cluster_stats.R_. Note that _cluster0_readstats.R_ by default downloads and cleans the data, so there is no need of running _"read_ushcn_daily.R"_ in advance. If the dataset is already available, this option can be removed in _cluster0_readstats.R_. The data is stored in the _data_ folder and the output produced is stored in the _output_ folder. The figures in the paper can then be produced by running the script _plot_stats_results.R_. 

The simulation study is performed by _synth.sh_, which launches two sbatch jobs. First a file _synth0.q_ runs the script _cluster0_readsynth.R_ which determines how many jobs are needed, and a second sbatch file _synth.q_ launches an array of jobs in parallel. Each of them runs the script _cluster_synth.R_.. The output produced is stored in the _output_ folder. The figures in the paper can then be produced by running the script _plot_synth_results.R_. 

## Notes on installing R and building the _hmevr_ package from source in a linux cluster 

The following steps may be system dependent. First, install _R_ and the required packages. I did build _R_ from source in my remote home directory. Before installing _R_, I run "$ _module load GCC/7.4.0_" as a c++ compiler is needed for compiling  the Stan models. _R_ must be built with the right GCC version, and In every session the _sbatch.q_ files included in the analysis load this correct GCC version before launching _R_ jobs.

If you need to install _devtools_ and _xml2_ on the cluster, you can use the following commands from within R:\
    > install.packages("xml2", configure.vars='INCLUDE_DIR=/usr/include/libxml2 LIB_DIR=/usr/lib64/')\
    > install.packages("devtools")\
    > use usethis::use_fun() instead of devtools::use_fun()\
    > usethis::use_package('rstan')\  
    
    
NOTE: do this before building _hmevr_ as rstan is necessary to compile the Stan models implemented in _hmevr_. Also, the other dependencies of _hmevr_ must be installed (see below for a complete list of dependencies).

Then, to build and install the _hmevr_ package (steps to repeat If the source code of the library changes):

I used the following shortcuts to push and pull my analysis to the cluster:

    cluster="ez23@dcc-something.something.duke.edu" 
    
    alias sendc="rsync -av --exclude='*.rds' /home/enrico/Projects/hmev2020si/codes/ 
    '${cluster}:~/../../group/mylab/ez23/hmev2020si/codes/'"
    
    alias sendhbevr="rsync -av --exclude src/ /home/enrico/Projects/hmevr/ 
    '${cluster}:~/../../group/mylab/ez23/hmevr/'"

Following steps from terminal:

1) $ ssh $cluster (ssh to remote cluster) 
2) $ cdhmev  (to cd to project folder)
4) $ cd hmevr (to cd to the R library folder)
5) $ module load GCC  (necessary for R and rstan)
6) $ R\
    > library(devtools)\
    > devtools::document() # NOT NECESSARY\
    > devtools::build(vignettes=FALSE)\
    > install.packages('../hmevr', repos = NULL, type="source")\
    
    (Here, one should also be able to use install_github("EnricoZorzetto/hmevr"))
    
7) Everything should work now: I can run one of the following commands
  + $ _bash kfold.sh_ to run the complete data analysis
  + $ _bash synth.sh_ to run the synthetic data analysis
  + $ _bash stats.sh_ to run the analysis of all the stations (NOT USED) 
  + $ _bash test.sh_ lighter job to make sure the cluster works (JUST A TEST)

8) Now, we can pull the results to our local machine, e.g., with the following commands
  + $ _pullout_
  + $ _pulloutsim_

Where

      alias pulloutsim='rsync -av "${cluster}:~/../../group/mylab/
             ez23/hmev2020si/output/output_data/output_synth/" 
                ~/Projects/hmev2020si/output/output_data/output_synth/'
                
      alias pullout='rsync -av "${cluster}:~/../../group/mylab/
             ez23/hmev2020si/output/output_data/" ~/Projects/hmev2020si/output/output_data/'
               

Note: When pushing the source code of the _hmevr_ library to remote, do not push the _src_ folder from local machine. If Stan code was modified, remove the remote src folder before rebuilding.

9) Once in the local machine, the output data can be used to produce the figures by running the following scripts:\
  + $ _ploty_stats_results.R_ (for the data analysis)\
  + $ _ploty_synth_results.R_ (for the simulation study)\


### Libraries and dependencies used by the code

The hmevr R package used here is provided in the Supplementary Materials. The package has the following dependencies:

hmevr Depends:
    R (>= 3.4.0)
hmevr Imports:
    Rcpp (>= 0.12.0),
    rstan (>= 2.18.1),
    rstantools (>= 2.0.0),
    extraDistr (>= 1.8.11),
    ggplot2 (>= 3.2.1),
    bayesplot (>= 1.7.0),
    nleqslv (>= 3.3.2)
    
    
    
Additionally, the R scripts reproducing simulation study and data analysis have the following additional dependencies:

ggplot2     3.2.1      \
data.table  1.12.8  \
gridExtra   2.3   \
dplyr       0.8.3 \
viridis     0.5.1   \
maps        3.3.0   \
stringr     1.4.0   \
mapdata     2.3.0  \
tseries     0.10-47 \
latex2exp   0.4.0    \
plyr        1.8.4 \
nleqslv     3.3.2  \
extraDistr  1.8.11 \
rstan       2.19.2 \
bayesplot   1.7.0 \
loo         2.1.0 \
reshape2    1.4.3 \
tidyr       1.0.0 




                                          
