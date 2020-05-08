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
library(tseries)
library(latex2exp)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


setwd( file.path('~','Projects','hbev','codes'))


source("hbev_module.R")    # main functions for data analysis
source("hbev_functions.R") # other functions
outplot = file.path('..', 'output', 'outplot')
dir.create(outplot, showWarnings = FALSE)

iter = 2000
chains = 4
ndraws = iter*chains/2
thresh_hbev = 0
adapt_delta = 0.8
corr_signif_lim = 0.05
trmin = 2
maxmiss = 30
min_nevents = 0
Nt = 366


datadir = file.path('..','data', 'Data_GHCN_Daily', 'extracted_csv')
# datadir = file.path('..','data', 'datasetsM')
filenames = list.files(datadir)

filepath = file.path(datadir, 'USW00094728.csv') # NEW YORK CENTRAL PARK
# filepath = file.path(datadir, 'Livermore.csv')
df = load_obs_data(filepath, maxmiss = maxmiss, min_nevents = min_nevents, dividebyten = TRUE , Nt = Nt)
res = table_max(df, Nt=366)


# use the first 50 years of the time series, and the rest 100 for validation
ssize = 10
datasets = split_obs_data(df, M_cal = ssize, 
                           M_val = 270, 
                           Nt = Nt,
                           cross_val = TRUE, 
                           reshuffle = FALSE, 
                           flip_time = FALSE,
                           reshuffle_days = FALSE,
                           decluster = TRUE,
                           signif_lim = corr_signif_lim)

data   = datasets$datacal$data
maxima = datasets$datacal$max
Mgen = dim(data)[1]

# modelname = 'wei_dyn_bin'
# params = c('mc', 'sc', 'mw', 'sw', 'pn')

modelname = 'mix_dyn_bin'
params = c('mc1', 'sc1', 'mw1', 'sw1', 'mc2', 'sc2', 'mw2', 'sw2', 'rhoj', 'pn')
priorpar1 = list(prior_w = 1.3, prior_c = 4, pn0_an = 4, pn0_bn = 16,
                 exp_mc10 = 1, exp_mw10 = 1, exp_sc10 = 0.1, exp_sw10 = 0.1,
                 exp_mc20 = 1, exp_mw20 = 1, exp_sc20 = 0.1, exp_sw20 = 0.1,
inf_mc10 = 200, inf_sc10 = 200, inf_sw10 = 200, inf_mw10 = 200,
inf_mc20 = 200, inf_sc20 = 200, inf_sw20 = 200, inf_mw20 = 200, rho0a = 200, rho0b = 200)
# priorpar2 = list(prior_w = 1.3, prior_c = 3, pn0_an = 4, pn0_bn = 16,
#                  exp_mc10 = 1, exp_mw10 = 1, exp_sc10 = 0.01, exp_sw10 = 0.01,
#                  exp_mc20 = 1, exp_mw20 = 1, exp_sc20 = 0.01, exp_sw20 = 0.01,
# inf_mc10 = 10, inf_sc10 = 10, inf_sw10 = 10, inf_mw10 = 10,
# inf_mc20 = 10, inf_sc20 = 10, inf_sw20 = 10, inf_mw20 = 10, rho0a = 60, rho0b = 60)

priorpar2 = list(exp_mw10 = 1)

# modelname = 'mix_dyp_bin'
# params = c('C1', 'w1', 'C2', 'w2', 'srho','mrho', 'pn')


# modelname = 'mix_sta_bin'
# params = c('C1', 'w1', 'C2', 'w2', 'rhoj', 'pn')

# paramsnames = unname(TeX( c('m_{1 \\delta}', 's_{1\\delta}', 'm_{1\\gamma}', 's_{1\\gamma}',
#                             'm_{2\\delta}', 's_{2\\delta}', 'm_{2\\gamma}', 's_{2\\gamma}', 
#                             '\\rho', '\\pi_0')))

# priorpar1 = list(prior_w = 0.7, prior_c = 6)
mixfit1 <- fit_ev_model(data, model = modelname, iter = iter, chains=chains, 
                       Mgen = Mgen, adapt_delta = adapt_delta, 
                       thresh_hbev = thresh_hbev, priorpar = priorpar1,
                       draw_prior = TRUE, refresh = 400)

# priorpar2 = list(prior_w = 0.5, prior_c = 1, pn0_an = 4, pn0_bn = 1,
#                  inf_mc = 10, inf_sc = 10, inf_sw = 10, inf_mw = 10)



# priorpar2 = list(prior_w = 1.3, prior_c = 12)

mixfit2 <- fit_ev_model(data, model = modelname, iter = iter, chains=chains, 
                       Mgen = Mgen, adapt_delta = adapt_delta, 
                       thresh_hbev = thresh_hbev, priorpar = priorpar2,
                       draw_prior = TRUE, refresh = 400)



# get posterior distributions:

get_priorpost <- function(mixfit, params){
  mixsim0 <- rstan::extract(mixfit$model_fit)
  priorsim <- as.data.frame(mixfit$prior$prior_rng)
  mynames <- colnames(priorsim)
  renamer <- function(x){ gsub("0prior", "", x ) }
  newnames <- lapply(mynames, renamer) 
  colnames(priorsim) <- newnames
  priorsim[['type']] = 'prior'
  # if ('alphaps' %in% names(mixsim0)){
  #   # colnames(mixsim0)[colnames() == "foo"] <- "bar"
  #   mixsim0[['rho']] <- mixsim0[['alphaps']]
  # } 
  mixsim = as.data.frame(mixsim0[params])
  mixsim[['type']] = sprintf('post: %s years', ssize)
  # mixsim[['ssize']] = ssize
  df = reshape2::melt(rbind(priorsim, mixsim), 
                      id.vars= c("type"),
                      variable.name = 'param',
                      value.name = 'value')
  return(df)
}

df1 = get_priorpost(mixfit1, params)
df1[['case']] <-  'A'
df2 = get_priorpost(mixfit2, params)
df2[['case']] <-  'B'
df = rbind(df1, df2)


  # df = reshape2::melt(rbind(priorsim, mixsim), 
  #                     id.vars= c("case"),
  #                     variable.name = 'param',
  #                     value.name = 'value')

fig <- ggplot(df) +
  # geom_histogram(aes(x=value, color = type)) +
  geom_density(aes(x=value, color = type, linetype=case)) +
  # coord_trans(x="log10") +
  # scale_x_continuous(trans='log10') +
  facet_wrap('param', scales = 'free') 
  
  fig
  figname = file.path('..', 'output', 'outplot', sprintf('prior_sens_%s_%s', modelname, ssize))
  ggsave(figname, plot=fig, device = 'png')


  # compute quantiles in the two cases:
mixq = comp_quant(mixfit1, maxima, trmin = trmin)
mixq2 = comp_quant(mixfit2, maxima, trmin = trmin)




p1 <- ggplot() +
  # geom_point(aes( res$Tr, res$Xi),  size = 3, color = "orange", shape = 1) +
  geom_point(aes( datasets$datacal$Tr, datasets$datacal$Xi),  size = 3, color = "black", shape = 2) +
  # geom_point(aes( datasets$dataval$Tr, datasets$dataval$Xi),  size = 3, color = "black", shape = 3) +
  
  geom_line(aes(  mixq$TrvalQ, mixq$qmean),  linetype="solid", color="red") +
  geom_line(aes(  mixq$TrvalQ, mixq$qupper), linetype="dashed",color="red") +
  geom_line(aes(  mixq$TrvalQ, mixq$qlower), linetype="dashed",color="red") +
  
  geom_line(aes(  mixq2$TrvalQ, mixq2$qmean),  linetype="solid",color= "green") +
  geom_line(aes(  mixq2$TrvalQ, mixq2$qupper), linetype="dashed",color="green") +
  geom_line(aes(  mixq2$TrvalQ, mixq2$qlower), linetype="dashed",color="green") +
  
  coord_trans(x="log10") +
  labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
  theme_bw()

p1
  figname2 = file.path('..', 'output', 'outplot', sprintf('prior_sens_quants_%s_%s', modelname, ssize))
  ggsave(figname2, plot=p1, device = 'png')
