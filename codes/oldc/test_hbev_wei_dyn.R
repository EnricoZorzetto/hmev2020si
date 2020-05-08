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
M = 100
Mgen = M # for plotting posterior only
# truew = 0.6
# truec = 8
actrue = 20
bctrue = 5
awtrue = 10
bwtrue = 15

mctrue = 10
sctrue = 3
mwtrue = 0.7
swtrue = 0.1
pntrue = 0.3
sdata = load_synth_data(M, 
                        # ptrue = list(ac = actrue, bc = bctrue, aw = awtrue,bw = bwtrue),
                        ptrue = list(mc = mctrue, sc = sctrue, mw = mwtrue, sw = swtrue),
                        # ptrue = list(w=truew, C = truec), 
                        # ptrue = list(w=truew, C = truec), 
                        ntrue = list(pn = pntrue), 
                        Nt = 366,
                        ndist = 'bin', dist = 'wei_dyn')

# fit the GEV model 
fit = fit_ev_model(sdata$data, model = 'wei_dyn_bin',  iter = niter, chains = nchains, Mgen = Mgen)

sim = rstan::extract(fit$model_fit)


quants = comp_quant(fit, sdata$maxima, trmin = 2)

max_sim = hbev_max_ppd(fit)





clrs <- color_scheme_get("brightblue")
ggplot() +
  geom_point(aes( quants$TrvalQ, quants$XivalQ),  size = 1.5, color = clrs[[6]]) +
  geom_line(aes(  quants$TrvalQ, quants$qmean),  linetype="solid",color="black") +
  geom_line(aes(  quants$TrvalQ, quants$qupper), linetype="dashed",color="black") +
  geom_line(aes(  quants$TrvalQ, quants$qlower), linetype="dashed",color="black") +
  coord_trans(x="log10") +
  labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
  theme_bw()


# plot the cdfs of the parameters:
# ggplot()+
# # p <- ggplot(df, aes(x=weight)) + 
#   geom_density(aes(x=sim$C), color="darkblue", fill="lightblue") + 
#   geom_vline(truec, 'dashed', 'blue', 4)

ppc1 <- ppc_dens_overlay( log(sdata$maxima), log(max_sim)[1:50,]) + labs(x="log(maxima)")


# ppc2 <- ppc_dens_overlay( rep(mctrue, 100) , sim$Cgen[1:50,]) + labs(x="C")+
#   vline_at(mctrue)

# true_pars = list(c = truec, w = truew)
true_pars = c(mc = mctrue, sc = sctrue, mw = mwtrue, sw = swtrue, pn = pntrue)
# true_pars = c(ac = actrue, bc = bctrue, aw = awtrue, bw = bwtrue)

# draws = as.data.frame(sim)[c("ac", "bc", "aw", "bw")]
draws = as.data.frame(sim)[c("mc", "sc", "mw", "sw", "pn")]
# draw_pars = list(draws, w = sim$wgen[1:50,1])


# plot priors:
priorlist = hbev_priors(fit$model, data = sdata$data, 
                        empirical = TRUE, thresh_hbev = fit$thresh_hbev,
                        draw_rng = TRUE, ndraws = 4000)

hist(priorlist$prior_rng$mc)

# priors = as.data.frame(priorlist$prior_rng)
priors = as.data.frame(priorlist$prior_rng)

# compare parameters draws with 'true' value
color_scheme_set("brightblue")
fig1 <-mcmc_recover_hist(draws, true_pars)  

# fig2 <-ppc_dens_overlay(priors['p0'], draws['p0'])



fig2 <- ggplot(gather(priors), aes(value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~key, scales = 'free_x')

grid.arrange(fig1, fig2)

# mcmc_recover_hist(, c(truec, truec))
# mcmc_recover_hist(, truec)


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






