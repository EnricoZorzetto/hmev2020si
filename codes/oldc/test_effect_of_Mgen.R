

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
library(hbevr)
options(mc.cores = parallel::detectCores())

# set seed for reproducible example:
set.seed(1)

################################################################################
# set parameters for analysis
niter = 2000
nchains = 4
ndraws = niter*nchains/2
# Mgen = 50 # number of sample points to average over latent level variables
trmin = 2 # minimum return times for which quantiles are computed
Nt = 366 # number of obs / block (days / year)

# generate synthetic data: length of validation and calibration datasets
Mval = 50
Mcal1 = 10
# Mcal2 = 50

# true parameters and model specification for generating datasets:
wtrue = 0.8
ctrue = 10
# actrue = 20
# bctrue = 5
# awtrue = 10
# bwtrue = 15
# 
mctrue = 5
sctrue = 0.5
mwtrue = 0.6
# mwtrue = 1
swtrue = 0.2
pntrue = 0.3

ptrue = list(mc = mctrue, sc = sctrue, mw = mwtrue, sw = swtrue)
# ptrue = list(C = ctrue, w = wtrue)
ntrue = list(pn = 0.3)

ndistr = 'bin'
pdistr = 'wei_dgu'
# pdistr = 'wei'
################################################################################

# generate data
cdata1 = load_synth_data(Mcal1,
            ptrue = ptrue,
            ntrue = ntrue,
            Nt = Nt,
            ndist = ndistr, dist = pdistr)

vdata = load_synth_data(Mval,
            ptrue = ptrue,
            ntrue = ntrue,
            Nt = Nt,
            ndist = ndistr, dist = pdistr)

# fit the EV models to the two samples 
dynfit0 = fit_ev_model(cdata1$data, model = 'wei_dgu_bin',  
                       iter = niter, chains = nchains, Mgen = 20)
dynq0 = comp_quant(dynfit0, vdata$maxima, trmin = trmin)

dynfit1 = fit_ev_model(cdata1$data, model = 'wei_dgu_bin',  
                       iter = niter, chains = nchains, Mgen = 50)
dynq1 = comp_quant(dynfit1, vdata$maxima, trmin = trmin)

dynfit2 = fit_ev_model(cdata1$data, model = 'wei_dgu_bin',  
                       iter = niter, chains = nchains, Mgen = 100)
dynq2 = comp_quant(dynfit2, vdata$maxima, trmin = trmin)



################################################################################


######################### compute TRUE distibution #############################

hbev_wei_pdf <- function(x, C = C, W = W, N = N){
  # only for scalar x!
  pdf = mean( N*pweibull(x, W, C)^(N-1)*dweibull(x, W, C))
return(pdf)
}

 
hbev_wei_cdf <- function(x, C = C, W = W, N = N){
  # only for scalar x!
  cdf <- mean(pweibull(x, W, C )^N)
  return(cdf)
}


hbev_wei_quant <- function(p, C = C, W = W, N = N){
  # only for scalar x!
  myfunq <-function(x) hbev_wei_cdf(x, C=C, W=W, N=N) - p
  F0 <- p
  x0 <- mean(C)*(log(mean(N)/(1-F0)))^(1/mean(W))
  optim <- nleqslv(x0, myfunq)
  quant <- optim$x
  return(quant)
}


dhbev <- function(x, ptrue = ptrue, ntrue = ntrue, 
                  pdistr = 'wei_dgu' , ndistr = 'bin',
                  Nt=366, nsamples = 50){
  # compute theoretical hbev cdf for the values in x (or Tr)
  # if nsamples = 1, draw from the static model with C=mc, W=mw
  
  if (ndistr == 'bin'){
      Ngen = rbinom(nsamples, Nt, ntrue$pn)
  } else if (ndistr == 'bbn') {
      Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
  }
  
  if (pdistr == 'wei_dgu'){
      Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
      Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
  } else if (pdistr == 'wei_dyn'){
      Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
      Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
  } else if (pdistr == 'wei'){
      Cgen = rep(nsamples, ptrue$C)
      Wgen = rep(nsamples, ptrue$w)
  }
    Nx = length(x)
    res = rep(0, Nx)
    for (i in 1:Nx){
      res[i] <-  hbev_wei_pdf(x[i], C=Cgen, W=Wgen, N=Ngen)
    }
    return(res)
}


phbev <- function(x, ptrue = ptrue, ntrue = ntrue, 
                  pdistr = 'wei_dgu' , ndistr = 'bin',
                  Nt=366, nsamples = 50, Tr = FALSE){
  # compute theoretical hbev cdf for the values in x (or Tr)
  # if nsamples = 1, draw from the static model with C=mc, W=mw
  
  if (ndistr == 'bin'){
      Ngen = rbinom(nsamples, Nt, ntrue$pn)
  } else if (ndistr == 'bbn') {
      Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
  }
  if (pdistr == 'wei_dgu'){
      Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
      Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
  } else if (pdistr == 'wei_dyn'){
      Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
      Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
  } else if (pdistr == 'wei'){
      Cgen = rep(nsamples, ptrue$C)
      Wgen = rep(nsamples, ptrue$w)
  }
    Nx = length(x)
    res = rep(0, Nx)
    for (i in 1:Nx){
      res[i] <-  hbev_wei_cdf(x[i], C=Cgen, W=Wgen, N=Ngen)
    }
    if (Tr == TRUE){
      res = 1/(1-res)
    }
    return(res)
}
qhbev <- function(fi, ptrue = ptrue, ntrue = ntrue, 
                  pdistr = 'wei_dgu' , ndistr = 'bin',
                  Nt=366, nsamples = 50, Tr = FALSE){
  # compute theoretical hbev quantiles for the values in fi.
  # values in fi must be non exceedance probabilities or return times 
  # (second option only if fromTr= FALSE)
  # if nsamples = 1, draw from the static model with C=mc, W=mw
  if (Tr == FALSE){
    Fi = fi
    if (!(max(Fi)<1)){
      print('phbev ERROR: check if Fi or Tr')
    }
  } else {
    Tr = fi
    Fi = 1-1/Tr
    if (!(min(Tr)>1)){
      print('phbev ERROR: check if Fi or Tr')
    }
  }
  if (ndistr == 'bin'){
      Ngen = rbinom(nsamples, Nt, ntrue$pn)
  } else if (ndistr == 'bbn') {
      Ngen = extraDistr::rbbinom(nsamples, Nt, alpha = ntrue$an, beta = ntrue$bn)
  }
  if (pdistr == 'wei_dgu'){
      Cgen = extraDistr::rgumbel(nsamples, mu = mctrue, sigma = sctrue)
      Wgen = extraDistr::rgumbel(nsamples, mu = mwtrue, sigma = swtrue)
  } else if (pdistr == 'wei_dyn'){
      Cgen = rgamma(nsamples, ptrue$ac, ptrue$bc)
      Wgen = rgamma(nsamples, ptrue$aw, ptrue$bw)
  } else if (pdistr == 'wei'){
      Cgen = rep(nsamples, ptrue$C)
      Wgen = rep(nsamples, ptrue$w)
  }
    Nx = length(fi)
    res = rep(0, Nx)
    for (i in 1:Nx){
      res[i] <- hbev_wei_quant(Fi[i], C=Cgen, W=Wgen, N=Ngen)
    }
    return(res)
}

rhbev <- function(n, ptrue = ptrue, ntrue = ntrue,
                  pdistr = 'wei_dgu' , ndistr = 'bin',
                  Nt=366, nsamples = 50){
  # draw n annual maxima from hbev distribution
  u <- runif(n)
  res <- qhbev(u, ptrue = ptrue, ntrue = ntrue,
               pdistr = pdistr, ndistr = ndistr,
               Nt = Nt, nsamples = nsamples)

  return(res)
}


xx = seq(20,400, 20)
fi = phbev(xx, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 500, Tr = FALSE)
qi = qhbev(fi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 500, Tr = FALSE)

plot(xx, qi)
lines(xx, xx)


fi = seq(0.05, 0.95, 0.05)
qi = qhbev(fi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 100, Tr = FALSE)
fhi = phbev(qi, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 100, Tr = FALSE)

plot( fi, fhi)
lines(fi, fi)

tr = seq(2, 50, 2)
qitr = qhbev(tr, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 5000, Tr = TRUE)
trh = phbev(qitr, ptrue = ptrue, ntrue = ntrue, ndistr = ndistr, pdistr = pdistr, 
           Nt = Nt, nsamples = 5000, Tr = TRUE)

plot( tr, trh)
lines(tr, tr)
dev.off()



################################################################################

plot(dynq0$Trval, dynq0$Xival, log = 'x')
lines(dynq0$TrvalQ, dynq0$qmean, col = 'red', lty  =1)
lines(dynq1$TrvalQ, dynq1$qmean, col = 'green', lty = 2)
lines(dynq2$TrvalQ, dynq2$qmean, col = 'blue', lty = 3)
lines(tr, qitr, col = 'black')
#