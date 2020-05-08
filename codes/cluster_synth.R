
rm(list=ls())  # remove existing workspace if any
init_time <- proc.time()

# import libraries
library(extraDistr) 
library(data.table)
library(rstan)
library(nleqslv)
library(hbevr)
rstan_options(auto_write = TRUE)


if (Sys.getenv('SLURM_CPUS_PER_TASK') == ""){
  use_cluster = FALSE
} else {
  use_cluster = TRUE
}

# read the jopb array ID and load the corresponding line of the csv file
# with the simulation specifications

if (use_cluster){
  
  # setwd( file.path('~','hbev','codes'))
  mc.cores <- as.integer(Sys.getenv('SLURM_CPUS_PER_TASK'))
  numj = Sys.getenv("SLURM_ARRAY_TASK_ID")
  numj = as.integer(numj)
  
} else {
  
  options(mc.cores = parallel::detectCores())
  # setwd( file.path('~','Projects','hbev','codes'))
  # numj = 3759
  numj = 5904
  
}

# read line numj of the csv file::



outdata =  file.path('..','output', 'output_data', 'output_synth')
dir.create(outdata, showWarnings = FALSE)
outplot =  file.path('..','output', 'outplot', 'outplot_synth')
dir.create(outplot, showWarnings = FALSE)


savename = file.path(outdata, sprintf('gof_synth_gener_%s.csv',numj))


mytable <- read.table(file.path('..','output', 'output_data',
                    'synth_list_of_jobs.csv'), sep = ',',
                    header = TRUE, row.names = 1)[numj, ]


# mytable <- read.table(file.path('..','output', 'output_data',
#                     'reduced_synth_list_of_jobs.csv'), sep = ',', 
#                     header = TRUE, row.names = 1)[numj, ]

myssize = as.double(mytable['ssize'][[1]])
myspec  = as.character(mytable['spec'][[1]])
mygen   = as.character(mytable['gen'][[1]])
mymodel = as.character(mytable['model'][[1]])
myindex = as.double(mytable['index'][[1]])
# check that myindex must be equal to numj

maxtree = list(wei_dgu_bin = 10, 
               wei_dyn_bin = 10, 
               wei_sta_bin = 10, 
               gev = 15, 
               pot_ppp = 10
               )

mymaxtree = maxtree[[mymodel]]

############### tunable parameters ####################
iter = 2000
chains = 4
Nt = 366          # number of obs / block
M_val = 500        # from all the remaining time series
# thresh_hbev = 0   # threshold for ordinary events
trmin = 2 # for computing quantiles only
# signif_lim = 0.05 # significative correlation values
Mgen = 50
# ssizes = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# ssizes = c(10, 20, 40, 50, 100)
# ssizes = c(10)
# gofs = c("lpml", "lppd", "fse", "elpd_loo", "p_loo")
gofs = c('mbias', "mwidth", "lpml", "lppd", "fse", 
         "elpd_waic2", 'p_waic2', "elpd_loo", "p_loo")
# models = c('gev', 'pot_ppp', 'wei_dyn_bin', 'wei_sta_bin', 'wei_dgu_bin')
# ngen = 3    # number of reshuffling / station /ssize
# specs = list(gpd="gpd", wei="wei", wei_dyn="wei_dgu", gam="gam")

# PTRUE = list(gpd=list(xi = 0.1, sigma = 10),
#              wei=list(w = 0.6, C = 8),
#              wei_dgu=list(mc = 7, sc = 1, mw = 0.8, sw = 0.1),
#              gam=list(a = 1.2, b = 0.1))

PTRUE = list(gpd=list(xi = 0.1, sigma = 8),
             wei=list(w = 0.6, C = 8),
             wei_dgu=list(mc = 6, sc = 1, mw = 1, sw = 0.1),
             gam=list(a = 1.2, b = 0.12))


ptrue = PTRUE[[myspec]]
ntrue = list(mn = 100, varn = 150)
# ntrue = list(an = 100, bn = 150)
# ntrue = list(pn = 0.4)
# ndist_true = 'bin'
ndist_true = 'bbn'
######################################################

savename = file.path(outdata,
   sprintf('gof_synth_gener_%s.csv',numj)) 


ngofs = length(gofs)

tests = c('ss', 'cv') # same sample and cross validation
ntests = length(tests)

rdf = expand.grid(test = tests, gof = gofs, numj = numj, ssize = myssize, 
                  spec = myspec, gen = mygen, 
                  model = mymodel , test_numj =myindex, value = NA)

    
      datacal = load_synth_data(myssize,
                  ptrue = ptrue,
                  ntrue = ntrue,
                  Nt = Nt,
                  ndist = ndist_true,
                  dist = myspec)
                
      fit = fit_ev_model(datacal$data, model=mymodel,
                  iter=iter, chains=chains, Mgen = Mgen, 
                  max_treedepth = mymaxtree)

      quants_ss = comp_quant(fit, datacal$max, trmin=trmin) # same sample
      
      mygofs_ss = list(lpml = quants_ss$lpml, 
                  lppd = quants_ss$lppd, 
                  fse = quants_ss$fse,
                  mbias = quants_ss$mbias,
                  mwidth = quants_ss$mwidth,
                  elpd_waic2 = quants_ss$elpd_waic2, 
                  p_waic2 = quants_ss$p_waic2, 
                  elpd_loo = quants_ss$elpd_loo, 
                  p_loo = quants_ss$p_loo)
      
      dataval = load_synth_data(M_val,
                  ptrue = ptrue,
                  ntrue = ntrue,
                  Nt = Nt,
                  ndist = ndist_true,
                  dist = myspec)

      quants_cv = comp_quant(fit, dataval$max, trmin=trmin) # cross validation
      
      mygofs_cv = list(lpml = quants_cv$lpml, 
                  lppd   = quants_cv$lppd, 
                  fse    = quants_cv$fse, 
                  mbias  = quants_cv$mbias,
                  mwidth = quants_cv$mwidth,
                  elpd_waic2 = quants_cv$elpd_waic2, 
                  p_waic2 = quants_cv$p_waic2, 
                  elpd_loo = quants_cv$elpd_loo, 
                  p_loo = quants_cv$p_loo)
      
for (iig in 1:ngofs){
    rdf$value[rdf$gof == names(mygofs_ss)[iig] 
            & rdf$test == 'ss'] = mygofs_ss[[iig]]
    rdf$value[rdf$gof == names(mygofs_cv)[iig] 
            & rdf$test == 'cv'] = mygofs_cv[[iig]]
}

      
print(savename)
write.csv(rdf, file = savename)


elapsed_time = proc.time() - init_time
sprintf("execution time was %s seconds", elapsed_time[3])

