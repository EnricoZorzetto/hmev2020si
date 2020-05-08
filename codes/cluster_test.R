# analyze single station and compute goodness of fit measures

rm(list=ls())  # remove existing workspace if any
init_time <- proc.time()

# import libraries
# library(VGAM) 
library(extraDistr) 
library(data.table)
library(rstan)
library(nleqslv)
library(ggplot2)
library(gridExtra)
# library(hbevr)
rstan_options(auto_write = TRUE)



if (Sys.getenv('SLURM_CPUS_PER_TASK') == ""){
  use_cluster = FALSE
} else {
  use_cluster = TRUE
}

args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied. Default no cross validation")
  ##supply default values: no cross validation
  kfold_cv = FALSE
  dataset = 'G' # default dataset
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
print(sprintf('kfold_cv = %s', kfold_cv))
print(sprintf('dataset = %s', dataset))


if (use_cluster){
  print('using cluster')  
  # setwd( file.path('~','hbev','codes'))
  mc.cores <- as.integer(Sys.getenv('SLURM_CPUS_PER_TASK'))
  numj = Sys.getenv("SLURM_ARRAY_TASK_ID")
  numj = as.integer(numj)
  
} else {
  
  options(mc.cores = parallel::detectCores())
  # setwd( file.path('~','Projects','hbev','codes'))
  numj = 16  # index of station record to read [16-Livermore]
  print('not using cluster')  
  
}




schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'cluster_test.stan', data = schools_dat)


print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)
