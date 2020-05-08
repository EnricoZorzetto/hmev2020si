# test gev and pot models
# using synthetic data


rm(list=ls()) 
# library(VGAM)
# # library(extRemes)
# library(rstan)
# library(ggplot2)
# library(bayesplot)
# library(gridExtra)
# library(loo)

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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


setwd( file.path('~','Projects','hbev','codes'))


source("hbev_module.R")    # main functions for data analysis
source("hbev_functions.R") # other functions
outplot = file.path('..', 'output', 'outplot', 'test_gev')
dir.create(outplot, showWarnings = FALSE)

niter = 2000
nchains = 4
ndraws = niter*nchains/2

# generate synthetic data:
M = 80
truew = 1
truec = 8

mctrue = 10
sctrue = 4
mwtrue = 1
swtrue = 0.2
varntrue = 160
mntrue = 100
sdata = load_synth_data(M, 
                        ptrue = list(mc=mctrue, sc=sctrue, mw=mwtrue, sw = swtrue), 
                        # ptrue = list(w=truew, C = truec), 
                        # ntrue = list(pn = 0.3), 
                        ntrue = list(mn = mntrue, varn = varntrue),
                        Nt = 366,
                        ndist = 'betabin', dist = 'wei_dyn')

# fit the GEV model 
wei_dyn_bbn_fit = fit_ev_model(sdata$data, model = 'wei_dyn_bbn',  iter = niter, chains = nchains, Mgen = 50)
wei_sta_bbn_fit = fit_ev_model(sdata$data, model = 'wei_sta_bbn',  iter = niter, chains = nchains, Mgen = 50)
wei_dyn_bin_fit = fit_ev_model(sdata$data, model = 'wei_dyn_bin', iter = niter, chains = nchains, Mgen = 50)
wei_dsc_bin_fit = fit_ev_model(sdata$data, model = 'wei_dsc_bin', iter = niter, chains = nchains, Mgen = 50)
wei_sta_bin_fit = fit_ev_model(sdata$data, model = 'wei_sta_bin', iter = niter, chains = nchains, Mgen = 50)
wei_dyl_bin_fit = fit_ev_model(sdata$data, model = 'wei_dyl_bin', iter = niter, chains = nchains, Mgen = 50)
wei_dsl_bin_fit = fit_ev_model(sdata$data, model = 'wei_dsl_bin', iter = niter, chains = nchains, Mgen = 50)
gam_dyn_bin_fit = fit_ev_model(sdata$data, model = 'gam_dyn_bin', iter = niter, chains = nchains, Mgen = 50)
gam_dsc_bin_fit = fit_ev_model(sdata$data, model = 'gam_dsc_bin', iter = niter, chains = nchains, Mgen = 50)
gam_sta_bin_fit = fit_ev_model(sdata$data, model = 'gam_sta_bin', iter = niter, chains = nchains, Mgen = 50)

wei_dyn_bbn_quants = comp_quant(wei_dyn_bbn_fit, sdata$maxima, trmin=2)
wei_sta_bbn_quants = comp_quant(wei_sta_bbn_fit, sdata$maxima, trmin=2)
wei_dyl_bin_quants = comp_quant(wei_dyl_bin_fit, sdata$maxima, trmin=2)
wei_dsl_bin_quants = comp_quant(wei_dsl_bin_fit, sdata$maxima, trmin=2)
wei_dyn_bin_quants = comp_quant(wei_dyn_bin_fit, sdata$maxima, trmin=2)
wei_dsc_bin_quants = comp_quant(wei_dsc_bin_fit, sdata$maxima, trmin=2)
wei_sta_bin_quants = comp_quant(wei_sta_bin_fit, sdata$maxima, trmin=2)
gam_dyn_bin_quants = comp_quant(gam_dyn_bin_fit, sdata$maxima, trmin=2)
gam_sta_bin_quants = comp_quant(gam_sta_bin_fit, sdata$maxima, trmin=2)
gam_dsc_bin_quants = comp_quant(gam_dsc_bin_fit, sdata$maxima, trmin=2)

wei_dyn_bbn_sim = rstan::extract(wei_dyn_bbn_fit$model_fit)
wei_sta_bbn_sim = rstan::extract(wei_sta_bbn_fit$model_fit)
wei_dyn_bin_sim = rstan::extract(wei_dyn_bin_fit$model_fit)
wei_dsc_bin_sim = rstan::extract(wei_dsc_bin_fit$model_fit)
wei_sta_bin_sim = rstan::extract(wei_sta_bin_fit$model_fit)
wei_dyl_bin_sim = rstan::extract(wei_dyl_bin_fit$model_fit)
wei_dsl_bin_sim = rstan::extract(wei_dsl_bin_fit$model_fit)
gam_dyn_bin_sim = rstan::extract(gam_dyn_bin_fit$model_fit)
gam_dsc_bin_sim = rstan::extract(gam_dsc_bin_fit$model_fit)
gam_sta_bin_sim = rstan::extract(gam_sta_bin_fit$model_fit)



clrs <- color_scheme_get("brightblue")
ggplot() +
  geom_point(aes( wei_sta_bbn_quants$TrvalQ, wei_sta_bbn_quants$XivalQ),  size = 1.5, color = clrs[[6]]) +
  geom_line(aes(  wei_sta_bbn_quants$TrvalQ, wei_sta_bbn_quants$qmean),  linetype="solid",color="black") +
  # geom_line(aes(  wei_sta_bbn_quants$TrvalQ, wei_sta_bbn_quants$qupper), linetype="dashed",color="black") +
  # geom_line(aes(  wei_sta_bbn_quants$TrvalQ, wei_sta_bbn_quants$qlower), linetype="dashed",color="black") +
  
  
  geom_line(aes( x= wei_dyn_bbn_quants$TrvalQ, y = wei_dyn_bbn_quants$qmean),  linetype="solid",color="red") +
  geom_line(aes( x= wei_dyn_bbn_quants$TrvalQ, y = wei_dyn_bbn_quants$qupper),  linetype="dashed",color="red") +
  geom_line(aes( x= wei_dyn_bbn_quants$TrvalQ, y = wei_dyn_bbn_quants$qlower),  linetype="dashed",color="red") +
  
  geom_line(aes(  wei_dsc_bin_quants$TrvalQ, wei_dsc_bin_quants$qmean),  linetype="solid",color= "green") +
  geom_line(aes(  wei_dyn_bin_quants$TrvalQ, wei_dyn_bin_quants$qmean),  linetype="solid",color= "orange") +
  geom_line(aes(  wei_sta_bin_quants$TrvalQ, wei_sta_bin_quants$qmean),  linetype="solid",color= "green") +
  
  geom_line(aes(  wei_dsl_bin_quants$TrvalQ, wei_dsl_bin_quants$qmean),  linetype="dashed",color="blue") +
  geom_line(aes(  wei_dyl_bin_quants$TrvalQ, wei_dyl_bin_quants$qmean),  linetype="dashed",color="blue") +

  # geom_line(aes(  gam_dyn_bin_quants$TrvalQ, gam_dyn_bin_quants$qmean),  linetype="solid",color="grey20") +
  # geom_line(aes(  gam_dsc_bin_quants$TrvalQ, gam_dsc_bin_quants$qmean),  linetype="solid",color="grey45") +
  geom_line(aes(  gam_sta_bin_quants$TrvalQ, gam_sta_bin_quants$qmean),  linetype="solid",color="grey60") +
  geom_line(aes(  gam_dyn_bin_quants$TrvalQ, gam_dyn_bin_quants$qmean),  linetype="solid",color="grey60") +
  geom_line(aes(  gam_dsc_bin_quants$TrvalQ, gam_dsc_bin_quants$qmean),  linetype="solid",color="grey60") +
  
  # geom_line(aes(  wei_dyn_bin_quants$TrvalQ, wei_dyn_bin_quants$qmean),  linetype="solid",color="blue") +
  # geom_line(aes(  wei_dsc_bin_quants$TrvalQ, wei_dsc_bin_quants$qmean),  linetype="solid",color="red") +
  # geom_line(aes(  wei_sta_bin_quants$TrvalQ, wei_sta_bin_quants$qmean),  linetype="solid",color="green") +
  # 
  
  coord_trans(x="log10") +
  labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
  theme_bw()



clrs <- color_scheme_get("brightblue")
ggplot() +
  geom_point(aes(TrvalQ, XivalQ),  data = wei_dyn_bbn_quants, size = 1.5, color = clrs[[6]]) +
  geom_line(aes(x=TrvalQ, qmean),  data = wei_dyn_bbn_quants, linetype="solid",color="black") +
  geom_line(aes(x=TrvalQ, qupper), data = wei_dyn_bbn_quants, linetype="dashed",color="black") +
  geom_line(aes(x=TrvalQ, qlower), data = wei_dyn_bbn_quants, linetype="dashed",color="black") +
  coord_trans(x="log10") +
  labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
  theme_bw()


# plot the cdfs of the parameters:
ggplot()+
# p <- ggplot(df, aes(x=weight)) + 
  geom_density(aes(x=wei_dyn_bin_sim$C), color="darkblue", fill="lightblue") + 
  geom_vline(truec, 'dashed', 'blue', 4)

ppc1 <- ppc_dens_overlay( rep(truec, times = ncol(wei_dyn_bbn_sim$C)), wei_dyn_bbn_sim$C) + labs(x="C")

# plot posterior distributions for the parameters:
color_scheme_set("blue")
posterior <- as.array(wei_dyn_bbn_fit$model_fit)
mcmc_dens(posterior, pars = c("mc", "sc", "mw", "sw", "mu", "omega"))

ppc_stat(rep(truec, M), wei_dyn_bbn_sim$C, stat='mean') # mean of the scale parameter
ppc_stat(rep(truec, M), wei_dyn_bbn_sim$C, stat='sd')   # stdv of the scale parameter


# gevsim = rstan::extract(gev_fit$model_fit)
# # posterior predictive check and LOO
# ppc1 <- ppc_dens_overlay(log(sample),log(gevsim$yrep[1:50,])) + labs(x="log(x)")
# # questa ha senso solo se Mgen = sample size:
# ppc2 <- ppc_stat(log(sample), log(gevsim$yrep), stat = "max") + labs(x="max(log(x))")
# psis2 <- psis(-gevsim$log_lik)
# clrs <- color_scheme_get("brightblue")
# pkhats <- ggplot() + geom_point(aes(x=seq(1,n),y=psis2$pareto_k), color=clrs[[5]]) + labs(y="khat", x="") +
#   geom_hline(yintercept=0, linetype="dashed") + ylim(-0.25,1) + theme_default()
# ppc3 <- ppc_loo_pit_qq(log(sample), log(gevsim$yrep), lw=psis2$lw_smooth)
# # grid.arrange(ppc1,ppc2,ppc3,pkhats,ncol=2)
# grid.arrange(ppc1,ppc2,pkhats,ncol=2)






