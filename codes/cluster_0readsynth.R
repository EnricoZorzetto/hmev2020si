
rm(list=ls())  # remove existing workspace if any
init_time <- proc.time()

# import libraries
library(extraDistr) 
library(data.table)
library(rstan)
library(nleqslv)
library(hmevr)

print("Running cluster_0readsynth code")
outdata1 =  file.path('..','output')
dir.create(outdata1, showWarnings = FALSE)
outdata =  file.path('..','output', 'output_data')
dir.create(outdata, showWarnings = FALSE)

########################## interation parameters ###############################

ssizes = c(10, 20, 40, 50, 100)
# ssizes = c(10, 20)
models = c('gev', 'pot_ppp', 'wei_dyn_bin', 'wei_sta_bin', 'wei_dgu_bin')
# specs = list(gpd="gpd", wei="wei", wei_dyn="wei_dgu", gam="gam")
specs = c("gpd","wei","wei_dgu","gam")

# gens = seq(2)
gens = seq(100)


savename = file.path(outdata, 'synth_list_of_jobs.csv') 

# sdf0 = data.frame(ssizes, models, specs)


sdf = expand.grid(ssize = ssizes, model = models, spec = specs, gen = gens)
sdf$index  = 1:nrow(sdf)
write.csv(sdf, file = savename)

# save number of jobs to file:
num_of_jobs = nrow(sdf)
fileConn<-file(file.path(outdata, "synth_number_of_jobs.txt"))
writeLines(as.character(num_of_jobs), fileConn)
close(fileConn)

################################################################################
