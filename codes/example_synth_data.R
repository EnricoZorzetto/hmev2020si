

################################################################################
####                                                                      ######
####   Example of application of the HBEV model to a simulated dataset    ######
####                                                                      ######
################################################################################


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
library(hmevr)
options(mc.cores = parallel::detectCores())

init_time = proc.time()
# set seed for reproducible example:
set.seed(1)

#  create folder for storing output if not there already
outplot1 = file.path('..', 'output')
dir.create(outplot1, showWarnings = FALSE)
outplot = file.path('..', 'output', 'outplot')
dir.create(outplot, showWarnings = FALSE)

################################################################################
###################    set parameters for analysis   ###########################
################################################################################
niter = 2000 # number of Iterations for each chain
nchains = 4 # number of chains
ndraws = niter*nchains/2 # number of posterior draws (half are burn-in period)
Mgen = 50 # number of sample points to average over latent level variables
trmin = 2 # minimum return times for which quantiles are computed
Nt = 366 # number of obs / block (days / year)
max_treedepth = 10 # for HMC sampler efficiency (use default)

# generate synthetic data: length of validation and calibration datasets
Mval = 200 # for validation
Mcal1 = 20 # for fitting (I)
Mcal2 = 50 # for fitting (II)

# true parameters
mctrue = 6 # Weibull scale expected value
sctrue = 1 # Weibull scale expected variability
mwtrue = 1 # Weibull shape expected value
swtrue = 0.1 # Weibull shape expected variability
pntrue = 0.3 # binomial rate for the number of events / block
ndistr = 'bin' # model for the number of events (binomial)
pdistr = 'wei_dgu' # model for the events magnitudes (Weibull)
################################################################################
myptrue = list(mc = mctrue, sc = sctrue, mw = mwtrue, sw = swtrue)
myntrue = list(pn = pntrue)
################################################################################

# Generate simulated data
cdata1 = load_synth_data(Mcal1,
            ptrue = myptrue,
            ntrue = myntrue,
            Nt = Nt,
            ndist = ndistr, dist = pdistr)

cdata2 = load_synth_data(Mcal2,
            ptrue = myptrue,
            ntrue = myntrue,
            Nt = Nt,
            ndist = ndistr, dist = pdistr)

# independent dataset for validation purposes
vdata = load_synth_data(Mval,
            ptrue = myptrue,
            ntrue = myntrue,
            Nt = Nt,
            ndist = ndistr, dist = pdistr)


# fit the EV models to the two samples 
dgu_prpar = list(gu_exp_sc0 = 0.25, gu_exp_sw0 = 0.05,
                 inf_sc0 =10, inf_sw0 = 10,
                inf_mc0 = 10, inf_mw0 = 10)
dynfit1 = fit_ev_model(cdata1$data, model = 'wei_dgu_bin',  
                       iter = niter, chains = nchains, Mgen = Mgen,
                       max_treedepth = max_treedepth, priorpar = dgu_prpar)

# stafit1 = fit_ev_model(cdata1$data, model = 'wei_sta_bin',  
#                      iter = niter, chains = nchains, Mgen = Mgen)

potfit1 = fit_ev_model(cdata1$data, model = 'pot_ppp',          
                       iter = niter, chains = nchains, Mgen = Mgen,
                       max_treedepth = max_treedepth)

gevfit1 = fit_ev_model(cdata1$data, model = 'gev',          
                       iter = niter, chains = nchains, Mgen = Mgen,
                       max_treedepth = max_treedepth)

dynfit2 = fit_ev_model(cdata2$data, model = 'wei_dgu_bin',  
                       iter = niter, chains = nchains, Mgen = Mgen, 
                       draw_priors = TRUE, 
                       max_treedepth = max_treedepth, priorpar = dgu_prpar)

# stafit2 = fit_ev_model(cdata2$data, model = 'wei_sta_bin',  
#                      iter = niter, chains = nchains, Mgen = Mgen)

potfit2 = fit_ev_model(cdata2$data, model = 'pot_ppp',          
                       iter = niter, chains = nchains, Mgen = Mgen,
                       max_treedepth = max_treedepth)

gevfit2 = fit_ev_model(cdata2$data, model = 'gev',          
                       iter = niter, chains = nchains, Mgen = Mgen, 
                       max_treedepth = max_treedepth)

# take a look at the results
# dynsim = rstan::extract(dynfit$model_fit)
# stasim = rstan::extract(stafit$model_fit)

# compute quantiles and goodness of fit measures
dynq1 = comp_quant(dynfit1, vdata$maxima, trmin = trmin)
# staq1 = comp_quant(stafit1, vdata$maxima, trmin = trmin)
potq1 = comp_quant(potfit1, vdata$maxima, trmin = trmin)
gevq1 = comp_quant(gevfit1, vdata$maxima, trmin = trmin)

dynq2 = comp_quant(dynfit2, vdata$maxima, trmin = trmin)
# staq2 = comp_quant(stafit2, vdata$maxima, trmin = trmin)
potq2 = comp_quant(potfit2, vdata$maxima, trmin = trmin)
gevq2 = comp_quant(gevfit2, vdata$maxima, trmin = trmin)


################################################################################
################################################################################
#### compute the real HBEV cdf for plotting purposes ###########################
# 
# hbev_wei_pdf <- function(x, C = C, W = W, N = N){
#   # only for scalar x!
#   pdf = mean( N*pweibull(x, W, C)^(N-1)*dweibull(x, W, C))
# return(pdf)
# }
# 
# 
# hbev_wei_cdf <- function(x, C = C, W = W, N = N){
#   # only for scalar x!
#   cdf <- mean(pweibull(x, W, C )^N)
#   return(cdf)
# }
# 
# 
# hbev_wei_quant <- function(p, C = C, W = W, N = N){
#   # only for scalar x!
#   myfunq <-function(x) hbev_wei_cdf(x, C=C, W=W, N=N) - p
#   F0 <- p
#   x0 <- mean(C)*(log(mean(N)/(1-F0)))^(1/mean(W))
#   optim <- nleqslv(x0, myfunq)
#   quant <- optim$x
#   return(quant)
# }
# 
# rhbev <- function(n, ptrue = ptrue, ntrue = ntrue,
#                   pdistr = 'wei_dgu' , ndistr = 'bin',
#                   Nt=366, nsamples = 50){
#   # draw n annual maxima from hbev distribution
#   u <- runif(n)
#   res <- qhbev(u, ptrue = ptrue, ntrue = ntrue,
#                pdistr = pdistr, ndistr = ndistr,
#                Nt = Nt, nsamples = nsamples)
# 
#   return(res)
# }
# 
# 
# dhbev <- function(x, ptrue = ptrue, ntrue = ntrue,
#                   pdistr = 'wei_dgu' , ndistr = 'bin',
#                   Nt=366, nsamples = 50){
#   # compute theoretical hbev cdf for the values in x (or Tr)
#   # if nsamples = 1, draw from the static model with C=mc, W=mw
# 
#   if (ndistr == 'bin'){
#       Ngen = rbinom(nsamples, Nt, ntrue$pn)
#   } else if (ndistr == 'bbn') {
#       Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
#   }
# 
#   if (pdistr == 'wei_dgu'){
#       Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
#       Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
#   } else if (pdistr == 'wei_dyn'){
#       Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
#       Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
#   } else if (pdistr == 'wei'){
#       Cgen = rep(nsamples, ptrue$C)
#       Wgen = rep(nsamples, ptrue$w)
#   }
#     Nx = length(x)
#     res = rep(0, Nx)
#     for (i in 1:Nx){
#       res[i] <-  hbev_wei_pdf(x[i], C=Cgen, W=Wgen, N=Ngen)
#     }
#     return(res)
# }
# 
# 
# phbev <- function(x, ptrue = ptrue, ntrue = ntrue,
#                   pdistr = 'wei_dgu' , ndistr = 'bin',
#                   Nt=366, nsamples = 50, Tr = FALSE){
#   # compute theoretical hbev cdf for the values in x (or Tr)
#   # if nsamples = 1, draw from the static model with C=mc, W=mw
# 
#   if (ndistr == 'bin'){
#       Ngen = rbinom(nsamples, Nt, ntrue$pn)
#   } else if (ndistr == 'bbn') {
#       Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
#   }
#   if (pdistr == 'wei_dgu'){
#       Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
#       Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
#   } else if (pdistr == 'wei_dyn'){
#       Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
#       Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
#   } else if (pdistr == 'wei'){
#       Cgen = rep(nsamples, ptrue$C)
#       Wgen = rep(nsamples, ptrue$w)
#   }
#     Nx = length(x)
#     res = rep(0, Nx)
#     for (i in 1:Nx){
#       res[i] <-  hbev_wei_cdf(x[i], C=Cgen, W=Wgen, N=Ngen)
#     }
#     if (Tr == TRUE){
#       res = 1/(1-res)
#     }
#     return(res)
# }
# 
# 
# qhbev <- function(fi, ptrue = ptrue, ntrue = ntrue,
#                   pdistr = 'wei_dgu' , ndistr = 'bin',
#                   Nt=366, nsamples = 50, Tr = FALSE){
#   # compute theoretical hbev quantiles for the values in fi.
#   # values in fi must be non exceedance probabilities or return times
#   # (second option only if fromTr= FALSE)
#   # if nsamples = 1, draw from the static model with C=mc, W=mw
#   if (Tr == FALSE){
#     Fi = fi
#     if (!(max(Fi)<1)){
#       print('phbev ERROR: check if Fi or Tr')
#     }
#   } else {
#     Tr = fi
#     Fi = 1-1/Tr
#     if (!(min(Tr)>1)){  
#       print('phbev ERROR: check if Fi or Tr')
#     }
#   }
#   if (ndistr == 'bin'){
#       Ngen = rbinom(nsamples, Nt, ntrue$pn)
#   } else if (ndistr == 'bbn') {
#       Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
#   }
#   if (pdistr == 'wei_dgu'){
#       Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
#       Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
#   } else if (pdistr == 'wei_dyn'){
#       Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
#       Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
#   } else if (pdistr == 'wei'){
#       Cgen = rep(nsamples, ptrue$C)
#       Wgen = rep(nsamples, ptrue$w)
#   }
#     Nx = length(fi)
#     res = rep(0, Nx)
#     for (i in 1:Nx){
#       res[i] <- hbev_wei_quant(Fi[i], C=Cgen, W=Wgen, N=Ngen)
#     }
#     return(res)
# }
############### move all that in HBEV package ##################

trtrue = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 30, 40, 50,75, 100, 125, 150, 200)
qutrue = qhbev(trtrue, ntrue = myntrue, ptrue = myptrue, nsamples = 500, 
               ndistr = ndistr, pdistr = pdistr, Nt = Nt, Tr = TRUE)

# rtrue = rhbev(10, ntrue = myntrue, ptrue = myptrue, nsamples = 20, 
#                ndistr = ndistr, pdistr = pdistr, Nt = Nt)


################################################################
# xx = seq(20,400, 20)
# fi = phbev(xx, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 500, Tr = FALSE)
# qi = qhbev(fi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 500, Tr = FALSE)
# 
# plot(xx, qi)
# lines(xx, xx)
# 
# 
# fi = seq(0.05, 0.95, 0.05)
# qi = qhbev(fi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 100, Tr = FALSE)
# fhi = phbev(qi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 100, Tr = FALSE)
# 
# plot( fi, fhi)
# lines(fi, fi)
# 
# tr = seq(20, 300, 20)
# qi = qhbev(tr, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 10000, Tr = TRUE)
# trh = phbev(qi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
#            Nt = Nt, nsamples = 10000, Tr = TRUE)
#
################################################################################
################################################################################
  
# clrs <- color_scheme_get("brightblue")
    
    linesize = 0.7
    shadealpha = 0.4
    # colmodels <- c("GEV"="red","PPP"="green","HBEV"="blue")
    p1 <- ggplot() +
      # geom_line(aes(gevq1$TrvalQ, gevq1$qmean),  
      #             linetype="solid",color= "red", size = linesize) +
        geom_ribbon(aes(x = gevq1$TrvalQ, 
                  ymax = gevq1$qupper, ymin = gevq1$qlower),  
                  alpha = shadealpha,  fill = "red") +
      geom_line(aes(gevq1$TrvalQ, gevq1$qupper), linetype="dotted",color="red") +
      geom_line(aes(gevq1$TrvalQ, gevq1$qlower), linetype="dotted",color="red") +
    
          geom_ribbon(aes(x = potq1$TrvalQ, 
                  ymax = potq1$qupper, ymin = potq1$qlower),  
                  alpha = shadealpha,  fill = "green") +
      geom_line(aes(potq1$TrvalQ, potq1$qupper), linetype="dotted",color="green") +
      geom_line(aes(potq1$TrvalQ, potq1$qlower), linetype="dotted",color="green") +
      
            geom_ribbon(aes(x = dynq1$TrvalQ, 
                  ymax = dynq1$qupper, ymin = dynq1$qlower),  
                  alpha = shadealpha,  fill = "blue") +
      geom_line(aes(dynq1$TrvalQ, dynq1$qupper), linetype="dotted",color="blue") +
      geom_line(aes(dynq1$TrvalQ, dynq1$qlower), linetype="dotted",color="blue") +
    
      geom_line(aes(gevq1$TrvalQ, gevq1$qmean, color= "red"),  
                linetype="dotdash", size = linesize) +
      geom_line(aes(potq1$TrvalQ, potq1$qmean, color= "green"),  
                linetype="twodash", size = linesize) +
      geom_line(aes(dynq1$TrvalQ, dynq1$qmean, color= "blue"),  
                linetype="solid",size = linesize) +
      
      
      scale_color_identity(element_blank(),
                           # name = "Model fit",
                           breaks = c("red", "green", "blue"),
                           labels = c("GEV", "POT", "HMEV"),
                           guide = "legend") +
      
        geom_point(aes( cdata1$Tr, cdata1$Xi), 
                color = 'black', shape = 1, size = 3.4, stroke = 1.2) +
      # geom_point(aes( vdata$Tr, vdata$Xi),
      #          size = 1.5,   color = 'black', shape = 2) +
      # geom_line(aes( tri_true, xxi),  linetype="solid",   
                # color = 'black', size = 1.2*linesize) +
        geom_line(aes( trtrue, qutrue),  linetype="solid",   
                color = 'black', size = 1.2*linesize) +
      # geom_line(aes( tri_true, xxi),  linetype="solid",   color = 'black') +
    
    
      annotate("text", x=2.5, y=180, label="a)", size = 8) +
      # coord_trans(x="log10", y='log10') +
      coord_trans(x="log10") +
      labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
      theme_bw() + 
    
      scale_x_continuous(breaks=c(2, 5, 10, 20, 50, 100, 200), limits = c(2, 202)) +
      # scale_y_continuous(limits=c(0, 200)) +
      theme(legend.position = c(0.35, 0.8),
            legend.background = element_blank(),
            text = element_text(size=14),
            legend.box.background = element_rect(colour = "black")
            ) 
    
    
    
    p2 <- ggplot() +
    
      
      geom_ribbon(aes(x = gevq2$TrvalQ, 
                  ymax = gevq2$qupper, ymin = gevq2$qlower),  
                  alpha = shadealpha,  fill = "red") +
      geom_line(aes(gevq2$TrvalQ, gevq2$qupper), linetype="dotted",color="red") +
      geom_line(aes(gevq2$TrvalQ, gevq2$qlower), linetype="dotted",color="red") +
      
        geom_ribbon(aes(x = potq2$TrvalQ, 
                  ymax = potq2$qupper, ymin = potq2$qlower),  
                  alpha = shadealpha,  fill = "green") +
      geom_line(aes(potq2$TrvalQ, potq2$qupper), linetype="dotted",color="green") +
      geom_line(aes(potq2$TrvalQ, potq2$qlower), linetype="dotted",color="green") +
      
          geom_ribbon(aes(x = dynq2$TrvalQ, 
                  ymax = dynq2$qupper, ymin = dynq2$qlower),  
                  alpha = shadealpha,  fill = "blue") +
      geom_line(aes(dynq2$TrvalQ, dynq2$qupper), linetype="dotted",color="blue") +
      geom_line(aes(dynq2$TrvalQ, dynq2$qlower), linetype="dotted",color="blue") +
      
      
      geom_line(aes(gevq2$TrvalQ, gevq2$qmean),  
                linetype="dotdash",color= "red",   size = linesize) +
      geom_line(aes(potq2$TrvalQ, potq2$qmean),  
                linetype="twodash",color= "green", size = linesize) +
      geom_line(aes(dynq2$TrvalQ, dynq2$qmean),  
                linetype="solid",color= "blue",  size = linesize) +
      
        geom_point(aes(cdata2$Tr, cdata2$Xi), 
                 color='black', shape=1, size = 3.4, stroke = 1.2) +
      # geom_point(aes( vdata$Tr, vdata$Xi),
      #          size = 1.5,   color = 'black', shape = 2) +
      # geom_line(aes( tri_true, xxi),  linetype="solid",   
                # color = 'black', size = 1.2*linesize) +
          geom_line(aes( trtrue, qutrue),  linetype="solid",   
                color = 'black', size = 1.2*linesize) +
    
    
      # coord_trans(x="log10", y='log10') +
      # annotate("text", x=2.5, y=max(gevq2$qupper), label="b)", size = 8) +
      # annotate("text", x=2.5, y=180, label="b)", size = 8) +
      coord_trans(x="log10") +
      labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
      theme_bw() +
    
      # scale_y_continuous(limits=c(0, 200)) +
      scale_x_continuous(breaks=c(2, 5, 10, 20, 50, 100, 200),   
                         limits = c(2, 202))+ 
    
          theme(
            # legend.position = c(0.35, 0.8),
            # legend.background = element_blank(),
            text = element_text(size=14),
            # legend.box.background = element_rect(colour = "black")
            ) 
    
    fig1 <- grid.arrange(p1, p2, ncol=2)
    # outplot = file.path('../output/outplot')
    ggsave(file.path(outplot, 'synth_examples_ss_20_50.png'), 
           plot=fig1, device = 'png', width = 8, height = 4.2)
    

################################################################################

################################################################################

# to compare pdf, need to generate data with the ssame length as Mgen
# gdata = load_synth_data(Mval,
#             ptrue = myptrue,
#             ntrue = myntrue,
#             Nt = Nt,
#             ndist = ndistr, dist = pdistr)

fit = dynfit2
quants = dynq2
sim = rstan::extract(fit$model_fit)
max_sim = hbev_max_ppd(fit)
sdata = cdata2

# clrs <- color_scheme_get("brightblue")
# ggplot() +
#   geom_point(aes(quants$TrvalQ, quants$XivalQ),  
#              size = 1.5, color = clrs[[6]]) +
#   geom_line(aes( quants$TrvalQ, quants$qmean),  
#             linetype="solid",color="black") +
#   geom_line(aes( quants$TrvalQ, quants$qupper), 
#             linetype="dashed",color="black") +
#   geom_line(aes( quants$TrvalQ, quants$qlower), 
#             linetype="dashed",color="black") +
#   coord_trans(x="log10") +
#   labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
#   theme_bw()

# PLOT PRIOR DISTR FOR THE DYNAMIC - GUMBEL MODEL

# plot the cdfs of the parameters:
# ggplot()+
# # p <- ggplot(df, aes(x=weight)) + 
#   geom_density(aes(x=sim$C), color="darkblue", fill="lightblue") + 
#   geom_vline(truec, 'dashed', 'blue', 4)

ppc1 <- ppc_dens_overlay( log(sdata$maxima), 
                          log(max_sim)[1:50,]) + labs(x="log(maxima)")


# ppc2 <- ppc_dens_overlay( rep(mctrue, 100) , sim$Cgen[1:50,]) + labs(x="C")+
#   vline_at(mctrue)

# true_pars = list(c = ctrue, w = wtrue)
true_pars = c(mc = mctrue, sc = sctrue, mw = mwtrue, sw = swtrue, pn = pntrue)
# true_pars = c(ac = actrue, bc = bctrue, aw = awtrue, bw = bwtrue)

# draws = as.data.frame(sim)[c("c",  "w")]
# draws = as.data.frame(sim)[c("ac", "bc", "aw", "bw")]
draws = as.data.frame(sim)[c("mc", "sc", "mw", "sw", "pn")]
# draw_pars = list(draws, w = sim$wgen[1:50,1])


# plot priors:
# priorlist = hbev_priors(fit$model, data = sdata$data, 
#                         empirical = TRUE, thresh_hbev = fit$thresh_hbev,
#                         draw_rng = TRUE, ndraws = 4000)

# hist(priorlist$prior_rng$mc)

# priors = as.data.frame(priorlist$prior_rng)
priors = as.data.frame(fit$prior$prior_rng)

# compare parameters draws with 'true' value
color_scheme_set("brightblue")
pp1 <-mcmc_recover_hist(draws, true_pars)  

# fig2 <-ppc_dens_overlay(priors['p0'], draws['p0'])



pp2 <- ggplot(gather(priors), aes(value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~key, scales = 'free_x')

fig2 <- grid.arrange(pp1, pp2)
ggsave(file.path(outplot, 'wei_synth_pdfs.png'), plot=fig2, device = 'png',
                       width = 6, height = 6)

# 
# listq = list(dynq = dynq, staq = staq)
# fig1 = plot_quants(listq)
# 
# 
# fig2 = plot_ppd_pdfs(dynfit, sdata$data, ndraws2plot = 100)
# 
# 
# # plot ppd pdf of parameters vs true values::
# 
# draws = as.data.frame(stasim)[c("C", "w", "pn")]
# 
# 
# # plot priors:
# priorlist = hbev_priors(stafit$model, data = sdata$data, 
#                         empirical = TRUE, thresh_hbev = stafit$thresh_hbev,
#                         draw_rng = TRUE, ndraws = 4000)
# 
# hist(priorlist$prior_rng$w)
# 
# # priors = as.data.frame(priorlist$prior_rng)
# priors = as.data.frame(priorlist$prior_rng)
# 
# # compare parameters draws with 'true' value
# color_scheme_set("brightblue")
# true_pars = c(ctrue, wtrue, pntrue)
# fig1 <-mcmc_recover_hist(draws, true_pars)  
# 
# # fig2 <-ppc_dens_overlay(priors['p0'], draws['p0'])
# 
# 
# 
# fig2 <- ggplot(gather(priors), aes(value)) + 
#   geom_histogram(nbins=30) + 
#   facet_wrap(~key, scales = 'free_x')
# 
# grid.arrange(fig1, fig2)
# 
elapsed_time = proc.time() - init_time
sprintf("execution time was %s seconds", elapsed_time[3])
