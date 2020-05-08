# read the stations to be analyze and produce csv file with list

rm(list=ls())  # remove existing workspace if any
library(data.table)
library(rstan)
library(hbevr)

Nt = 366
minlength = 100 # keep only stations longer than this value [number of years]
minlength_daily = 10 # keep only stations longer than this value for daily analysis
minlength_50 = 50 # keep only stations longer than this value for daily analysis
min_nevents = 0
maxmiss = 30 # max number of missing obs / year
nshuffles_kfold = 10


######################### SET ANALYSIS PARAM ###################################
################################################################################

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
  dataset = 'G' # default the GRl dataset
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
print(sprintf('kfold_cv = %s', kfold_cv))
print(sprintf('dataset = %s', dataset))

if (use_cluster){
  # setwd( file.path('~','hbev','codes'))
} else {
  setwd( file.path('~','Projects','hbev','codes'))
}
# source("hbev_module.R")    # main functions for data analysis
# source("hbev_functions.R") # other functions
outdata = file.path('..', 'output', 'output_data')

# get the directory with the correct dataset:
if (dataset == 'M'){
    datadir = file.path('..','data', 'datasetsM')
} else if (dataset == 'K'){
    datadir = file.path('..','data', 'datasetsK')
} else if (dataset == 'G'){
    # datadir = file.path('..','data', 'Data_GHCN_CONUS_csv', 'stations')
    datadir = file.path('..','data', 'Data_GHCN_Daily', 'extracted_csv')
} else {
  print('Cluster stats ERROR -> specify a valid dataset!')
}

filenames_all = list.files(datadir, pattern = "\\.csv$")# values set in hbev_functions
nfiles = length(filenames_all)
# read stations here and keep only those with _at least_ 100 years of data
NLS = array(0, nfiles)
# LAT = array(0, nfiles)
# LON = array(0, nfiles)
for (i in 1:nfiles){
# for (i in 1:5){
  print(i)
  filepath = file.path(datadir, filenames_all[i])
  df = load_obs_data(filepath, maxmiss = maxmiss, 
                     min_nevents = min_nevents, dividebyten = TRUE , Nt = Nt)
  
  if (dim(df)[1]>0){
    # df = load_obs_data(filepath, maxmiss = maxmiss, 
    #                  min_nevents = min_nevents, dividebyten = TRUE , Nt = Nt)
    res = table_max(df, Nt=Nt)# values set in hbev_functions
    NLS[i] = res$nyears
  } else{
    NLS[i] = 0
  }
  
  # # do not 
  # if (dataset == 'G'){
  #   dfstats = read.csv( file.path('..', 'data', 'Data_GHCN_Daily','list_ushcn_stats.csv'))
  #   myname = unlist(strsplit(filenames_all[i], '.csv'))
  #   dfii = subset(dfstats, dfstats$STATION == myname)
  #   LON[i] = dfstats$LON[i]
  #   LAT[i] = dfstats$LAT[i]
  # }
}
  # print(sprintf('defining priors for the the %s model', model))
  # mod_names = unlist(strsplit(model, "_"))
  # n_mod = mod_names[[3]]
  # x_mod = paste(mod_names[[1]], mod_names[[2]], sep="_")

# for extreme value analysis, only long datasets
filenames = filenames_all[NLS >= minlength]
dfs <- as.data.frame(filenames)
numberofstats = length(filenames)

# for daily pdf analysis, almost all datasets
################################################################################
filenames_daily = filenames_all[NLS >= minlength_daily] 
dfs_daily <- as.data.frame(filenames_daily)
numberofstats_daily = length(filenames_daily)
fileConn<-file(file.path(outdata, "numberofstats_daily.txt"))
writeLines(as.character(numberofstats_daily), fileConn)
close(fileConn)
write.table(dfs_daily, file = file.path(outdata, sprintf('list_stats_daily_%s.csv',
                             numberofstats_daily)), col.names = FALSE, sep = ',')
################################################################################  

# for analysis from 50-years or more
################################################################################
filenames_50 = filenames_all[NLS >= minlength_50] 
dfs_50 <- as.data.frame(filenames_50)
numberofstats_50 = length(filenames_50)
fileConn<-file(file.path(outdata, "numberofstats_50y.txt"))
writeLines(as.character(numberofstats_50), fileConn)
close(fileConn)
fileConn<-file(file.path(outdata, "numberofjobs_50y.txt"))
writeLines(as.character(numberofstats_50), fileConn)
close(fileConn)
write.table(dfs_50, file = file.path(outdata, sprintf('list_stats_50y%s.csv',
                             numberofstats_50)), col.names = FALSE, sep = ',')
################################################################################  

# write.table(dfs, file = file.path(outdata, sprintf('list_stats_%s.csv',
#                                numberofstats)), col.names = FALSE, sep = ',')
# fileConn<-file(file.path(outdata, "numberofstats.txt"))
# writeLines(as.character(numberofstats), fileConn)
# close(fileConn)


#   if (dataset == 'G'){
# dfpos_all = data.frame(filenames_all, NLS, LAT, LON)
# write.table(dfpos_all, file = file.path(outdata, sprintf('list_stats_pos_all_%s.csv',
#                                numberofstats)), col.names = NA, sep = ',')
# dfpos = subset(dfpos_all, dfpos_all$NLS >= minlength)
# # dfpos_all = data.frame(filenames_all, NLS, LAT, LON)
# write.table(dfpos, file = file.path(outdata, sprintf('list_stats_pos_%s.csv',
#                                numberofstats)), col.names = NA, sep = ',')
# }

filenames_cv = rep(filenames, nshuffles_kfold)
dfs_cv <- as.data.frame(filenames_cv)
numberofstats_cv = length(filenames_cv)
# write.table(dfs_cv, file = file.path(outdata, sprintf('list_stats_%s_cv.csv', 
#                               numberofstats)), col.names = FALSE, sep = ',')
# fileConn_cv <- file(file.path(outdata, "numberofstats_cv.txt"))
# writeLines(as.character(numberofstats_cv), fileConn_cv)
# close(fileConn_cv)

if (kfold_cv == FALSE){
  fileConn<-file(file.path(outdata, "numberofstats.txt"))
  writeLines(as.character(numberofstats), fileConn)
  close(fileConn)
  write.table(dfs, file = file.path(outdata, sprintf('list_stats_%s.csv',
                               numberofstats)), col.names = FALSE, sep = ',')

  fileConn<-file(file.path(outdata, "numberofjobs.txt"))
  writeLines(as.character(numberofstats), fileConn)
  close(fileConn)
  # dfpos = data.frame(filenames, NLS, LAT, LON)
  # write.table(dfpos, file = file.path(outdata, 
  #                             sprintf('list_stats_pos_%s.csv', 
  #                             numberofstats)), col.names = NA, sep = ',')
  
} else {
    fileConn<-file(file.path(outdata, "numberofstats_cv.txt"))
  writeLines(as.character(numberofstats), fileConn)
  close(fileConn)
  write.table(dfs_cv, file = file.path(outdata, sprintf('list_stats_%s_cv.csv', 
                             numberofstats)), col.names = FALSE, sep = ',')
  fileConn_cv <- file(file.path(outdata, "numberofjobs_cv.txt"))
  writeLines(as.character(numberofstats_cv), fileConn_cv)
  close(fileConn_cv)
  # dfpos = data.frame(filenames, NLS, LAT, LON)
  # write.table(dfpos, file = file.path(outdata, 
  #                             sprintf('list_stats_pos_%s_cv.csv', 
  #                             numberofstats)), col.names = NA, sep = ',')
  
}




