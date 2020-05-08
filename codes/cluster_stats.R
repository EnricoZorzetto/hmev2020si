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
library(hbevr)
rstan_options(auto_write = TRUE)


############### tunable parameters ####################

iter = 2000
chains = 4
Nt = 366          # number of obs / block
# M_val = 200        # from all the remaining time series
thresh_hbev = 0   # threshold for ordinary events
trmin = 2 # for computing quantiles only
# signif_lim = 0.05 # significative correlation values
Mgen = 50
# ssizes = c(10)
save_plots = TRUE
decluster_signif_lim = 0.1
maxmiss = 30
min_nevents = 0
do_declustering = TRUE

# ssizes = c(10)
gofs = c("lpml", "lppd", "fse", "elpd_loo", "p_loo", "mwidth",
         'elpd_waic2', 'p_waic2', 'mbias', 
         'trmax_width', 'trmax_quant', 'trmin_quant')

# models = c('wei_dgu_bin', 'wei_dyn_bin', 'wei_sta_bin', 'gev', 'pot_ppp')
models = c('wei_dgu_bin', 'gev', 'pot_ppp')

maxtree = list(wei_dgu_bin = 10, 
             wei_dyn_bin = 10, 
             wei_sta_bin = 10, 
             gev = 15, 
             pot_ppp = 10
             )


# sample size for the analysis:
ssizes_samesample = c(10, 20, 30, 40, 50)
ssizes_kfold = c(10, 20, 30, 40, 50)
# ssizes_kfold = c(20, 50)

if (do_declustering == TRUE){
  decflag = 'dec'
} else {
  decflag = 'nodec'
}




if (Sys.getenv('SLURM_CPUS_PER_TASK') == ""){
  use_cluster = FALSE
} else {
  use_cluster = TRUE
}

args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied. Default is cross validation")
  ##supply default values: no cross validation
  kfold_cv = TRUE
  dataset = 'G' # default dataset
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}
print(sprintf("using cluster = %s", use_cluster))
print(sprintf('kfold_cv = %s', kfold_cv))
print(sprintf('dataset = %s', dataset))


if (use_cluster){
  
  # setwd( file.path('~','hbev','codes'))
  mc.cores <- as.integer(Sys.getenv('SLURM_CPUS_PER_TASK'))
  numj = Sys.getenv("SLURM_ARRAY_TASK_ID")
  numj = as.integer(numj)
  
} else {
  
  options(mc.cores = parallel::detectCores())
  # setwd( file.path('~','Projects','hbev','codes'))
  numj = 1929  # index of station record to read
  
}




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

# source("hbev_module.R")    # main functions for data analysis
# source("hbev_functions.R") # other functions used in the project
  outdata_list = file.path('..', 'output', 'output_data')

# read number of SINGLE stations to name output file::


if (kfold_cv == TRUE){
  testflag = 'kfold'
  print('kfold cross validation')
    ssizes = ssizes_kfold
    M_val = 50
    # nfolds = nshuffles_kfold
    flip_time = FALSE
    reshuffle_years = TRUE
    cross_val = TRUE
    fileConnW<-file(file.path(outdata_list, "numberofstats_cv.txt"))
    nsinglestats =readLines(fileConnW)
    close(fileConnW)

    fileConnR<-file(file.path(outdata_list, "numberofjobs_cv.txt"))
    numstats =readLines(fileConnR)
    close(fileConnR)

    dfs = read.table(file.path(outdata_list, sprintf('list_stats_%s_cv.csv', 
                   nsinglestats)), col.names = c('index', 'station'), sep =',')
    # outdata =  file.path('..','output', 'output_data', 
    #                      sprintf('output_kfold_%s', nsinglestats))
    # outdata_warn =  file.path('..','output', 'output_data', 
    #                    sprintf('warnings_kfold_%s', nsinglestats))
    # outdata_logging =  file.path('..','output', 'output_data', 
    #                    sprintf('logging_kfold_%s', nsinglestats))

    filename <- as.character(dfs$station[[numj]])
    
    # savename = file.path(outdata, sprintf('gof_kfold_%s_%s', numj, filename))
    # savewarn = file.path(outdata_warn, sprintf('warn_kfold_%s_%s', numj, filename))
    


} else {
  print('stats analysis')
  testflag = 'ssample'
    ssizes = ssizes_samesample
    M_val = 300
    # nfolds = 1
    flip_time = TRUE
    reshuffle_years = FALSE
    cross_val = TRUE
    
    fileConnW<-file(file.path(outdata_list, "numberofstats_50y.txt"))
    nsinglestats =readLines(fileConnW)
    close(fileConnW)
    
    # fileConnW<-file(file.path(outdata_list, "numberofstats.txt"))
    # nsinglestats =readLines(fileConnW)
    # close(fileConnW)
    
    # fileConnR<-file(file.path(outdata_list, "numberofjobs.txt"))
    # numstats =readLines(fileConnR)
    # close(fileConnR)
    
    numstats = nsinglestats

    dfs = read.table(file.path(outdata_list, sprintf('list_stats_50y%s.csv', 
                       nsinglestats)), col.names = c('index', 'station'), sep =',')
        
    # dfs = read.table(file.path(outdata_list, sprintf('list_stats_%s.csv', 
    #                    nsinglestats)), col.names = c('index', 'station'), sep =',')
    
    # outdata =  file.path('..','output', 'output_data', 
    #                    sprintf('output_stats_%s', nsinglestats))
    # outdata_warn =  file.path('..','output', 'output_data', 
    #                    sprintf('warnings_stats_%s', nsinglestats))
    # outdata_logging =  file.path('..','output', 'output_data', 
    #                    sprintf('logging_stats_%s', nsinglestats))
    # filename <- as.character(dfs$station[[numj]])
    
    # savename = file.path(outdata, sprintf('gof_stats_%s_%s',numj, filename))
    # savewarn = file.path(outdata_warn, sprintf('warn_stats_%s_%s', numj, filename))
}
  
# folders to save results
outdata =  file.path('..','output', 'output_data', 
                sprintf('output_%s_%s_%s_%s', testflag, 
                nsinglestats, thresh_hbev, decflag))
outdata_warn =  file.path('..','output', 'output_data', 
                sprintf('warnings_%s_%s_%s_%s', testflag, 
                nsinglestats, thresh_hbev, decflag))
outdata_logging =  file.path('..','output', 'output_data', 
                sprintf('logging_%s_%s_%s_%s' , testflag, 
                nsinglestats, thresh_hbev, decflag))
    
dir.create(outdata, showWarnings = FALSE)
dir.create(outdata_warn, showWarnings = FALSE)
dir.create(outdata_logging, showWarnings = FALSE)

# name for saving results 
savename = file.path(outdata, sprintf('gof_%s_%s_%s', testflag, numj, filename))
savewarn = file.path(outdata_warn, sprintf('warn_%s_%s_%s', testflag, numj, filename))

ww <- file( file.path(outdata_logging, sprintf("warning_logging_%s_%s.out", 
                                               numj, filename)),  open="wt")
sink(file=ww, append=TRUE, type="message")



# plot only in the case of same-sample analysis
outplot =  file.path('..','output', 'outplot', 
                     sprintf('outplot_stats_%s', nsinglestats))
dir.create(outplot, showWarnings = FALSE)


tests = c('ss', 'cv') # same-sample or cross-validation

#### load station data ###############################
# filenames = list.files(file.path('..','data', 'datasetsM'))
# filenames = list.files(file.path('..','data', 'Data_GHCN_CONUS_csv', 'stations'))
filepath = file.path(datadir, filename)

# filepath = file.path('..', 'data','Data_GHCN_CONUS_csv', 'stations', 'USC00426135.csv')
df = load_obs_data(filepath, maxmiss = maxmiss, min_nevents = min_nevents, 
                   dividebyten = TRUE , Nt = Nt)
######################################################

statname = unlist(strsplit(filename, '.csv'))[[1]]






nssizes = length(ssizes)
ngofs = length(gofs)
nmodels = length(models)
ntests = length(tests)
plotsize = ssizes[nssizes] # size at which to plot quantiles



# initialize dataframe for saving results
mycube <- array(0,
          dim = c(nssizes, ntests, nmodels, ngofs),
          dimnames = list(ssize = ssizes, test = tests, model = models, gof = gofs))

# initialize dataframe to save diagnostic values
diagn = c('n_warn_x0', 'n_diverg', 'n_max_tree', 'n_low_bfmi') 
ndiagn = length(diagn)
mycube2 <- array(0,
          dim = c(nssizes, nmodels, ndiagn),
          dimnames = list(ssize = ssizes, model = models, diagn = diagn))

# mycube <- array(0,
#           dim = c(nssizes, nmodels, ngofs, nfolds),
#           dimnames = list(ssize = ssizes, model = models, gof = gofs, fold = folds))

dfr = as.data.table(mycube, value.name = 'score')
dfr = cbind(dfr, station = statname) # save station name 
dfr = cbind(dfr, numj = numj) # save generation 

dfd = as.data.table(mycube2, value.name = 'number')
dfd = cbind(dfd, station = statname) # save station name 
dfd = cbind(dfd, numj = numj) # save generation 



  
for (iis in 1:nssizes){
    
      
      data = split_obs_data(df, M_cal = ssizes[iis], 
                   M_val = M_val, 
                   Nt = Nt,
                   cross_val = cross_val, 
                   reshuffle = reshuffle_years, 
                   flip_time = flip_time, # start with samples from recent years
                   reshuffle_days = FALSE,
                   decluster = do_declustering,
                   signif_lim = decluster_signif_lim)
      
      

        listq = list()
        for (iim in 1:nmodels){
          
          #added:
          n_err_max = 1
          while (n_err_max > 0){
          
          fit_ss = fit_ev_model(data$datacal$data, model=models[iim],
                             iter=iter, chains=chains, Mgen = Mgen, 
                             thresh_hbev = thresh_hbev, 
                             max_treedepth = maxtree[[models[iim]]])
          
          # compute goodness of fit both for same sample (ss) and all data (ad)
          quants_ss = comp_quant(fit_ss, data$datacal$max, trmin=trmin)
          quants_cv = comp_quant(fit_ss, data$dataval$max, trmin=trmin)
          
          mygofs_cv = list(lpml = quants_cv$lpml, 
                           lppd = quants_cv$lppd, 
                           fse = quants_cv$fse, 
                           mbias = quants_cv$mbias, 
                           elpd_loo = quants_cv$elpd_loo,
                           p_loo = quants_cv$p_loo,
                           mwidth = quants_cv$mwidth,
                           p_waic2 = quants_cv$p_waic2,
                           elpd_waic2 = quants_cv$elpd_waic2,
                           trmax_width = tail(quants_cv$width_Tr, n = 1),
                           trmax_quant = tail(quants_cv$qmean, n = 1),
                           trmin_quant = quants_cv$qmean[1])
          
          mygofs_ss = list(lpml = quants_ss$lpml, 
                           lppd = quants_ss$lppd, 
                           fse = quants_ss$fse, 
                           mbias = quants_ss$mbias, 
                           elpd_loo = quants_ss$elpd_loo,
                           p_loo = quants_ss$p_loo,
                           mwidth = quants_ss$mwidth,
                           p_waic2 = quants_ss$p_waic2,
                           elpd_waic2 = quants_ss$elpd_waic2,
                           trmax_width = tail(quants_ss$width_Tr, n = 1),
                           trmax_quant = tail(quants_ss$qmean, n = 1),
                           trmin_quant = quants_ss$qmean[1])
          
          # save diagnostic quantities:
          n_warn_x0 = quants_ss$nwarning_x0
          n_diverg = get_num_divergent(fit_ss$model_fit)
          n_max_tree = get_num_max_treedepth(fit_ss$model_fit)
          n_low_bfmi = length(get_low_bfmi_chains(fit_ss$model_fit))
          
          # check no divergences occurred and repeat if necessary
          n_err_max = max(n_diverg, n_max_tree, n_low_bfmi)
          
          
          } # end of while - section of code to repeat if divergence occur
         # write down results now: 
          
    dfd[ssize == ssizes[iis] & model == models[iim] 
                             & diagn == 'n_warn_x0' ]$number = n_warn_x0
    dfd[ssize == ssizes[iis] & model == models[iim] 
                             & diagn == 'n_diverg'  ]$number = n_diverg
    dfd[ssize == ssizes[iis] & model == models[iim] 
                             & diagn == 'n_max_tree']$number = n_max_tree
    dfd[ssize == ssizes[iis] & model == models[iim] 
                             & diagn == 'n_low_bfmi']$number = n_low_bfmi

    # save qq-plots
    if (save_plots == TRUE & ssizes[iis] == plotsize & kfold_cv == FALSE){
     listq[[iim]] = quants_ss 
    }
          
    for (iig in 1:ngofs){
      
      dfr[ssize == ssizes[iis]
        & model == models[iim]   
        & test == 'ss'   # same-sample
        & gof   == names(mygofs_ss)[iig]]$score = mygofs_ss[[iig]]
      
      dfr[ssize == ssizes[iis]
        & model == models[iim]   
        & test == 'cv'   # cross-validation
        & gof   == names(mygofs_cv)[iig]]$score = mygofs_cv[[iig]]
        # & gof   == names(mygofs_ss)[iig]]$score = mygofs_cv[[iig]]
      
    }
      
        } # loop on models
        
        # if plot, save qqplots for current station:
          if (save_plots == TRUE & ssizes == plotsize & kfold_cv == FALSE){
        fig <- plot_quants(listq)
        figname <- file.path(outplot, sprintf('quantiles_%s_%s.pdf', statname, numj))
        ggsave(figname, plot=fig, device = 'pdf')
          } 
        
       } # loop on ssizes

print(savename)
write.csv(dfr, file = savename)

print(savewarn)
write.csv(dfd, file = savewarn)

elapsed_time = proc.time() - init_time
sprintf("execution time was %s seconds", elapsed_time[3])

close(ww) # close logging file
  
