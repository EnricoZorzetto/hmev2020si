# test gev and pot models
# using real data


rm(list=ls()) 
library(nleqslv)
library(extraDistr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(rstan)
library(bayesplot)
library(loo)
library(VGAM)
library(evd)
library(hmevr)
library(tseries)

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())


# setwd( file.path('~','Projects','hbev','codes'))


# source("hbev_module.R")    # main functions for data analysis
# source("hbev_functions.R") # other functions
# outplot = file.path('..', 'output', 'outplot', 'test_gev')
# dir.create(outplot, showWarnings = FALSE)

iter = 2000
chains = 4
ndraws = iter*chains/2
thresh_hbev = 0.0
adapt_delta = 0.8
corr_signif_lim = 0.05
trmin = 5
Mgen = 50
Mcal =10
fliptime =FALSE
decluster = TRUE
reshuffle = TRUE

# # generate synthetic data:
# M = 40
# truew = 1
# truec = 8
# sdata = load_synth_data(M, 
#                         ptrue = list(w=truew, C = truec), 
#                         ntrue = list(p0 = 0.3), 
#                         Nt = 366,
#                         ndist = 'bin', dist = 'wei')

# check record lengths:

# filenames = list.files(file.path('..','data', 'datasetsK'), pattern = "\\.csv$")
# nfiles = length(filenames)
# NL <- array(0, nfiles)
# for (i in 1:nfiles){
# filepath = file.path('..', 'data', 'datasetsK', filenames[i])
# print(filepath)
# df = load_obs_data(filepath, maxmiss = 30, min_nevents = 1, dividebyten = TRUE , Nt = 366)
# res = table_max(df, Nt=366)
# NL[i] = res$nyears
# }
# 
# years = unique(df$YEAR)
# nyears = length(years)
# Nobs = array(0, nyears)
# for (i in 1:nyears){
#  Nobs[i] = dim( subset(df, df$YEAR == years[i]))[1] 
# }
# A =  subset(df, df$YEAR == 1804)

# filenames = list.files(file.path('..','data', 'datasetsK'), pattern = "\\.csv$")
# filenames = list.files(file.path('..','data', 'datasetsM'), pattern = "\\.csv$")
# datadir = file.path('..','data', 'datasetsM')

# datadir = file.path('..','data', 'Data_GHCN_CONUS_csv', 'stations')
datadir = file.path('..','data', 'Data_GHCN_Daily', 'extracted_csv')
filenames = list.files(datadir)
# 

  
# filepath = file.path('..', 'data','Data_GHCN_CONUS_csv', 'stations', 'USC00478027.csv')
# filepath = file.path('..', 'data','Data_GHCN_CONUS_csv', 'stations', 'USC00298387.csv')
# filepath = file.path(datadir, 'USW00094728.csv') # NYCP
# filepath = file.path(datadir, 'USC00311677.csv') # CHAPEL HILL
# filepath = file.path(datadir, 'USC00044232.csv')

# filepath = file.path(datadir, 'USC00112348.csv') # problem check
# filepath = file.path(datadir, 'USC00027281.csv') # problem check
filepath = file.path(datadir, 'USC00112348.csv') # problem check

df = load_obs_data(filepath, maxmiss = 30, min_nevents = 0, dividebyten = TRUE , Nt = 366)
  res = table_max(df, Nt=366)

nobs = length(df$PRCP)


hist(res$totals/res$N)

# lag = 10
# x1 = log(df$PRCP[1:(nobs-lag)])
# x2 = log(df$PRCP[(1+lag):nobs])
# plot(x1,x2)

# means = res$totals/res$N
# means = res$N
# means = res$totals
means = df$PRCP

lag = 1
x1 =means[1:(length(means)-lag)]
x2 =means[(1+lag):length(means)]
plot(x1,x2)

cor(x1, x2)


# first 50 years of the time series
datasets = split_obs_data(df, M_cal = Mcal,
                           M_val = 270, 
                           Nt = 366,
                           cross_val = FALSE, 
                           reshuffle = reshuffle, 
                           flip_time = fliptime,
                           reshuffle_days = FALSE,
                           decluster = decluster,
                           signif_lim = corr_signif_lim)

datasetsdec = split_obs_data(df, M_cal = Mcal,
                           M_val = 270, 
                           Nt = 366,
                           cross_val = FALSE, 
                           reshuffle = reshuffle, 
                           flip_time = fliptime,
                           reshuffle_days = FALSE,
                           decluster = decluster,
                           signif_lim = corr_signif_lim)

data = datasets$datacal$data
datadec = datasetsdec$datacal$data
maxima = datasets$dataval$max

# print(datasets$datacal$lagtau)

# plot(df$YEAR)

# library(sn)
# # plot(1, 1, xlim = c(0, 3))
# dev.off()
# curve(dexp(x, 1/(1*0.1)), add = TRUE, col = 'red', from = 0, to = 1)
# curve(dsn(x, 0, 1*0.05, 5), add = TRUE)
# 
# curve(dgamma(x, 10*6, 6), to = 30, col = 'red')
# curve(dinvgamma(x, 6, 6*10), add = TRUE)
# # curve(dsn(x, 10, 1, 5), add = TRUE, col = 'green')
# Mgen = dim(data)[1]

# 
# priorpar_mid = list(inf_mc20 = 100, inf_sc10 = 100, inf_mw20 = 100, inf_sw10 = 100,
#                     inf_mc20 = 100, inf_sc20 = 100, inf_mw20 = 100, inf_sw20 = 100,
#                     exp_mc10 = 0.8, exp_mw10 = 1, exp_mc20 = 1.2, exp_mw20 = 1,
#                     rho0a = 100, rho0b = 100)
# midfit <- fit_ev_model(data, model = 'mix_dyn_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

# midfit <- fit_ev_model(data, model = 'mix_dyn_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# 
# miofit <- fit_ev_model(data, model = 'mix_dyn_bin_oldsave', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# 
# oldfit <- fit_ev_model(data, model = 'mix_sta_bin_oldsave', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

# 
# copfit <- fit_ev_model(data, model = 'cop_dyn_bin', iter = iter, chains=  chains ,
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# 
# copsim = rstan::extract(copfit$model_fit)

# dylfit <- fit_ev_model(data, model = 'wei_dyl_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

stafit <- fit_ev_model(data, model = 'wei_sta_bin', iter = iter, chains=chains, 
           Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

potfit <- fit_ev_model(data, model = 'pot_ppp', iter = iter, chains=chains,
           Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

gevfit <- fit_ev_model(data, model = 'gev', iter = iter, chains=chains,
           Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# dexfit <- fit_ev_model(data, model = 'wei_dex_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

      # exp_sc0 = 0.3, # as a fraction of expected mc
      # exp_sw0 = 0.1, # as 
      
# prsn = list(gu_exp_sc0 = 0.20, gu_exp_sw0 = 0.05)
prsn = list(gu_exp_sc0 = 0.30, gu_exp_sw0 = 0.1, # by default 0.25, 0.05
                 inf_sc0 = 10, inf_sw0 = 10, # by default is 100
                inf_mc0 = 10, inf_mw0 = 10) # by default is 10

dsnfit <- hmevr::fit_ev_model(data, model = 'wei_dgu_bin', iter = iter, chains=chains , priorpar = prsn,
           Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
dsnq <- comp_quant(dsnfit, maxima, trmin = trmin)

# 
# dsnfit <- fit_ev_model(data, model = 'wei_dgu_bin', iter = iter, chains=chains,
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# 
# dsnq <- comp_quant(dsnfit, maxima, trmin = trmin)

# dynsim = rstan::extract(dynfit$model_fit)

  # # wei_dsn_bin model
  # inf_snloc = 10,
  # inf_snsc = 200,
  # inf_snsh = 200,
  # sn_cloc = 0.8, # as a fraction of expected values - not needed for now
  # sn_wloc = 0.9, # as a fraction of expected values - not needed for now
  # sn_charvar_c = 0.2,
  # sn_charvar_w = 0.1,
  # sn_shape_c = 10,
  # sn_shape_w = 10

# dexsim = rstan::extract(dexfit$model_fit)
# dsnsim = rstan::extract(dsnfit$model_fit)

# dyndfit <- fit_ev_model(datadec, model = 'wei_dyn_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# 


# stadfit <- fit_ev_model(datadec, model = 'wei_sta_bin', iter = iter, chains=chains , 
#            Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

# misfit <- fit_ev_model(data, model = 'mix_sta_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)
# mipfit <- fit_ev_model(data, model = 'mix_dyp_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev, refresh = 400)

# midq = comp_quant(midfit, maxima, trmin = trmin)
# 
# misq <- comp_quant(misfit, maxima, trmin = trmin)
# midq <- comp_quant(midfit, maxima, trmin = trmin)
# mioq <- comp_quant(miofit, maxima, trmin = trmin)
# dynq <- comp_quant(dynfit, maxima, trmin = trmin)
# dexq <- comp_quant(dexfit, maxima, trmin = trmin)
staq <- comp_quant(stafit, maxima, trmin = trmin)
potq <- comp_quant(potfit, maxima, trmin = trmin)
gevq <- comp_quant(gevfit, maxima, trmin = trmin)

# dyndq <- comp_quant(dyndfit, maxima, trmin = trmin)
# stadq <- comp_quant(stadfit, maxima, trmin = trmin)


# print(dynq$lppd)
print(dsnq$lppd)
print(staq$lppd)
print(potq$lppd)
print(gevq$lppd)

print(dsnq$fse)
print(staq$fse)
print(potq$fse)
print(gevq$fse)


print(dsnq$mbias)
print(staq$mbias)
# print(dexq$lppd)
# print(dyndq$lppd)
# print(stadq$lppd)
  

# print(dynq$fse)
# print(staq$fse)
# print(dyndq$fse)
# print(stadq$fse)


# dynsim = rstan::extract(dynfit$model_fit)
# midsim = rstan::extract(midfit$model_fit)
# hist(dynsim$alphap)

# gamfit <- fit_ev_model(data, model = 'gam_dyn_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta)
# dylfit <- fit_ev_model(data, model = 'wei_dyl_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)
# dynfit <- fit_ev_model(data, model = 'wei_dyn_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)
# stafit <- fit_ev_model(data, model = 'wei_sta_bin', iter = iter, chains=chains , Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)
# potfit <- fit_ev_model(data, model = 'pot_ppp', iter =iter, chains=chains     ,  Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)
# gevfit <- fit_ev_model(data, model = 'gev', iter =iter, chains=chains         ,  Mgen = Mgen, adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)

# plot time serie of daily rainfall in NYC
# png(file.path(outplot, 'NYCP_time_series.png'), width = 600, height = 400)
# plot(res$year, res$max, xlab = 'year', ylab = 'Annual maximum daily rainfall [mm/day]', xaxt="n", 
#      main = 'Daily rainfall time series recorded at NEW YORK CNTRL PK TWR \n (ID USW00094728)')
# # lines(res$year, res$max)
# lines(df$DATE/10000, df$PRCP)
# xtick<-seq(1840, 2010, by=20)
# axis(side=1, at=xtick)
# dev.off()
#   
# ndiv = sum(get_divergent_iterations(dynfit$model_fit))
# print(ndiv)
# 
# ndiv = sum(get_divergent_iterations(midfit$model_fit))
# print(ndiv)
# 
# ndiv = sum(get_divergent_iterations(potfit$model_fit))
# print(ndiv)
#   
# ndiv = sum(get_divergent_iterations(gevfit$model_fit))
# print(ndiv)
# 
# 
# # nmtd = sum(get_max_treedepth_iterations(dynfit$model_fit))
# # nlpi = get_num_leapfrog_per_iteration(dynfit$model_fit)
# 
# 
# 
# 
# get_bfmi(dynfit$model_fit)



# gevsim = extract(gevfit$model_fit)
# dynsim = extract(dynfit$model_fit)
# stasim = extract(stafit$model_fit)
# pairs(gevsim, pars = c("psi", "k", 'mu'))
# pairs(stasim, pars = c("C", "w"))
# pairs(dynsim, pars = c("ac", "bc"))
#   
# gevq = comp_quant(gevfit, maxima, trmin = trmin)
# potq = comp_quant(potfit, maxima, trmin = trmin)
# dynq = comp_quant(dynfit, maxima, trmin = trmin)
# staq = comp_quant(stafit, maxima, trmin = trmin)
  
# print goodness of fit measures::
# print(gevq$elpd_loo)
# print(potq$elpd_loo)
# print(dynq$elpd_loo)
# print(staq$elpd_loo)
# 
# print(staq$elpd_waic1)
# print(staq$elpd_waic2)
# print(staq$p_waic1)
# print(staq$p_waic2)
# 
# print(gevq$lpml)
# print(potq$lpml)
# print(dynq$lpml)
# print(staq$lpml)
# 
# print(gevq$lppd)
# print(potq$lppd)
# print(dynq$lppd)
# print(staq$lppd)
# 
# print(misq$lppd)
# print(midq$lppd)
# print(mipq$lppd)
# 
# print(gevq$fse)
# print(potq$fse)
# print(dynq$fse)
# print(staq$fse)
# 
# print(midq$fse)
# 
# dynq$qmean
# staq$qmean


# loo_gev = loo::loo(gevq$log_lik)
# loo_pot = loo::loo(potq$log_lik)
# loo_dyn = loo::loo(dynq$log_lik)
# loo_sta = loo::loo(staq$log_lik)

# loo::compare(loo_gev, loo_pot) # res diff = -3, SE 3.4 -> FIRST IS BETTER IF NEGATIVE
# loo::compare(loo_dyn, loo_sta)

p1 <- ggplot() +
  geom_point(aes( res$Tr, res$Xi),  size = 3, color = "orange", shape = 1) +
  geom_point(aes( datasets$datacal$Tr, datasets$datacal$Xi),  size = 3, color = "cyan", shape = 2) +
  geom_point(aes( datasets$dataval$Tr, datasets$dataval$Xi),  size = 3, color = "black", shape = 3) +
  
  # geom_point(aes( gevq$TrvalQ, gevq$XivalQ),  size = 1.5, color = "black") +
  geom_line(aes(  staq$TrvalQ, staq$qmean),  linetype="solid", color="orange") +
  geom_line(aes(  staq$TrvalQ, staq$qupper), linetype="dashed",color="orange") +
  geom_line(aes(  staq$TrvalQ, staq$qlower), linetype="dashed",color="orange") +

  # geom_line(aes(  dexq$TrvalQ, dexq$qmean),  linetype="solid", color="red") +
  # geom_line(aes(  dexq$TrvalQ, dexq$qupper), linetype="dashed",color="red") +
  # geom_line(aes(  dexq$TrvalQ, dexq$qlower), linetype="dashed",color="red") +
  # # # # 
  # geom_line(aes(  dynq$TrvalQ, dynq$qmean),  linetype="solid",color= "blue") +
  # geom_line(aes(  dynq$TrvalQ, dynq$qupper), linetype="dashed",color="blue") +
  # geom_line(aes(  dynq$TrvalQ, dynq$qlower), linetype="dashed",color="blue") +
  
  geom_line(aes(  potq$TrvalQ, potq$qmean),  linetype="solid",color= "green") +
  geom_line(aes(  potq$TrvalQ, potq$qupper), linetype="dashed",color="green") +
  geom_line(aes(  potq$TrvalQ, potq$qlower), linetype="dashed",color="green") +
  
  geom_line(aes(  gevq$TrvalQ, gevq$qmean),  linetype="solid",color= "red") +
  geom_line(aes(  gevq$TrvalQ, gevq$qupper), linetype="dashed",color="red") +
  geom_line(aes(  gevq$TrvalQ, gevq$qlower), linetype="dashed",color="red") +

  #
  geom_line(aes(  dsnq$TrvalQ, dsnq$qmean),  linetype="solid",color= "blue") +
  geom_line(aes(  dsnq$TrvalQ, dsnq$qupper), linetype="dashed",color="blue") +
  geom_line(aes(  dsnq$TrvalQ, dsnq$qlower), linetype="dashed",color="blue") +

coord_trans(x="log10", y = 'log10') +
  labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
  theme_bw()
p1
p2 <- ggplot() +
  geom_point(aes( res$Xi, res$Fi),  size = 3, color = "orange", shape = 1) +
  geom_point(aes( datasets$datacal$Xi, datasets$datacal$Fi),  size = 3, color = "cyan", shape = 2) +
  geom_point(aes( datasets$dataval$Xi, datasets$dataval$Fi),  size = 3, color = "black", shape = 3) +

  # geom_line(aes(  dynq$Xival, dynq$fupper), linetype="dashed",color="blue") +
  # geom_line(aes(  dynq$Xival, dynq$fmean),  linetype="solid",color= "blue") +
  # geom_line(aes(  dynq$Xival, dynq$flower), linetype="dashed",color="blue") +
  
  geom_line(aes(  dsnq$Xival, dsnq$fmean),  linetype="solid",color= "blue") +
  geom_line(aes(  dsnq$Xival, dsnq$fupper), linetype="dashed",color="blue") +
  geom_line(aes(  dsnq$Xival, dsnq$flower), linetype="dashed",color="blue") +
  
  # geom_line(aes(  dexq$Xival, dexq$fmean),  linetype="solid",color= "red") +
  # geom_line(aes(  dexq$Xival, dexq$fupper), linetype="dashed",color="red") +
  # geom_line(aes(  dexq$Xival, dexq$flower), linetype="dashed",color="red") +
  # 
  geom_line(aes(  staq$Xival, staq$fmean),  linetype="solid",color= "orange") +
  geom_line(aes(  staq$Xival, staq$fupper), linetype="dashed",color="orange") +
  geom_line(aes(  staq$Xival, staq$flower), linetype="dashed",color="orange") +
  
  geom_line(aes(  gevq$Xival, gevq$fmean),  linetype="solid",color= "red") +
  geom_line(aes(  gevq$Xival, gevq$fupper), linetype="dashed",color="red") +
  geom_line(aes(  gevq$Xival, gevq$flower), linetype="dashed",color="red") +
  
  geom_line(aes(  potq$Xival, potq$fmean),  linetype="solid",color= "green") +
  geom_line(aes(  potq$Xival, potq$fupper), linetype="dashed",color="green") +
  geom_line(aes(  potq$Xival, potq$flower), linetype="dashed",color="green") +

coord_trans(x="log10") +
  labs(y = "P(X < x)", x= "Quantile x [mm/day]") +
  theme_bw()
p2
# fig <- grid.arrange(p1, p2, ncol=2)

# dynsim = rstan::extract(dynfit$model_fit)

# hist(dynsim$mc)
# hist(dynsim$sc)
# hist(dynsim$mw)
# hist(dynsim$sw)


# clrs <- color_scheme_get("brightblue")

# 
# 
# listq = list(gevq, potq, dynq, staq)
# fig1 = plot_quants(listq)
# plot(fig1)
# 
# 
# fig2 <- plot_ppd_pdfs(dynfit, datasets$datacal$data, ndraws2plot = 50)
# plot(fig2)
# ggsave(file.path(outplot, 'example_ppdf_pdf.png'), plot=fig2, device = 'png',
#                        width = 14, height = 6)
# library(ggplot2)
# 
# line.data <- data.frame(x=seq(0, 10, length.out=10), y=runif(10, 0, 10))
# 
# qplot(Sepal.Length, Petal.Length, color=Species, data=iris) +
#   geom_line(aes(x, y, color="My Line"), data=line.data)

# dfd = decluster(df, tau_max = 5, signif_lim = 0.05)
# 
# plot(df$PRCP[1:365])
# points(dfd$decdata$PRCP[1:365], col = 'red')
# 
# plot(datasets$dataval$N)
# plot(datasets$dataval$max)


