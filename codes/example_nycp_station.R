
################################################################################
####                                                                      ######
#### Example of application of the HBEV model to the New York time series ######
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
library(tseries)
library(hmevr)
library(latex2exp)
library(reshape2)
library(showtext)
showtext_auto()

######################## analysis parameters ###################################
iter = 2000 # number of Iterations for each chain
chains = 4  # number of chains
ndraws = iter*chains/2 # number of posterior draws (half are burn-in period)
thresh_hbev = 5 # threshold used to select wet events in the HMEV model
adapt_delta = 0.8 # default target acceptance rate for HMC sampler
corr_signif_lim = 0.1 # correlation threshold used to decluster time series
trmin = 2 # minimum value of return time for which quantiles are computed
maxmiss = 30 # maximum number of missing/flagged data points/year accepted
min_nevents = 0 # minimum number of non-zero events/year accepted 
Nt = 366 # number of events / block = number of days/year
decluster = FALSE # If TRUE, perform declustering before fit
Mgen = 50 # number of sample points for HBEV latent parameters
sample_cal_1 = 20
sample_cal_2 = 50
hmevmodel = 'wei_dgu_bin'
# hmevmodel = 'wei_dyn_bin'
# gevmodel = 'wei_sta_bin'
potmodel = 'pot_ppp'
gevmodel = 'gev'
reshuffle_years = TRUE
################################################################################


# create folder for saving results If does not exists already
outplot1 = file.path('..', 'output')
dir.create(outplot1, showWarnings = FALSE)
outplot = file.path('..', 'output', 'outplot')
dir.create(outplot, showWarnings = FALSE)


# Read data
# If data is not available, you can download it and process it 
# by running the script read_ushcn_data.R
# alternatively the dataset can be loaded from the hmevr as df = hmevr::nycp
datadir = file.path('..','data', 'Data_GHCN_Daily', 'extracted_csv')
filenames = list.files(datadir)
stat_id = 'USW00094728'# NEW YORK CENTRAL PARK
filepath = file.path(datadir, sprintf('%s.csv', stat_id)) 
# filepath = file.path(datadir, 'USC00311677.csv') # CHAPEL HILL

df = load_obs_data(filepath, maxmiss = maxmiss, 
           min_nevents = min_nevents, dividebyten = TRUE , Nt = Nt)
# df = hmevr::nycp # alternatively, the NYCP data is also in the hmevr package
res = table_max(df, Nt=Nt)

# hist(datasets1$dataval$max)


# Selecting only the first 20 or 50 years of the time series

datasets1 = split_obs_data(df, M_cal = sample_cal_1, 
                           M_val = 270, 
                           Nt = Nt,
                           cross_val = FALSE, 
                           reshuffle = reshuffle_years, 
                           flip_time = FALSE,
                           reshuffle_days = FALSE,
                           decluster = decluster,
                           signif_lim = corr_signif_lim)

# # Selecting only the first 20 or 50 years of the time series
# datasetsD = split_obs_data(df, M_cal = 20, 
#                            M_val = 270, 
#                            Nt = Nt,
#                            cross_val = FALSE, 
#                            reshuffle = FALSE, 
#                            flip_time = FALSE,
#                            reshuffle_days = FALSE,
#                            decluster = FALSE,
#                            signif_lim = corr_signif_lim)
# 
# 
# A = datasets1$datacal$data[1, ]
# B = datasetsD$datacal$data[1, ]
# 
# plot(A)
# points(B, col='red')

# datasets1$datacal$lagtau
# datasets1$datacal$lagtau

# Calibration dataset: extract all daily data and annual maxima
data1   = datasets1$datacal$data
maxima1 = datasets1$dataval$max

# Prior parameters for the HBEV model 
# (if not assigned, the default values are used)
dgu_prpar = list(gu_exp_sc0 = 0.25,  gu_exp_sw0 = 0.05, # only for gumbel latent level
                 inf_sc0 =10, inf_sw0 = 10,
                 inf_mc0 = 10, inf_mw0 = 10)

# fit the HMEV model

# hmevmodel = 'wei_dyn_bin'
dynfit1 <- hmevr::fit_ev_model(data1, model = hmevmodel,
                       iter = iter, chains=chains,
                       Mgen = Mgen, adapt_delta = adapt_delta,
                       thresh_hbev = thresh_hbev, priorpar = dgu_prpar)


# dynfit1P <- hmevr::fit_ev_model(data1, model = hmevmodel,
#                        iter = iter, chains=chains,
#                        Mgen = Mgen, adapt_delta = adapt_delta,
#                        thresh_hbev = 0.0, priorpar = dgu_prpar)

# pairs(dynfit1$model_fit, pars=c("mw", "mc", 'pn'))
# pairs(dynfit1P$model_fit, pars=c("mw", "mc", 'pn'))

# # fit the equivalent static model
# stafit1 <- hmevr::fit_ev_model(data1, model = 'wei_sta_bin', 
#                        iter = iter, chains=chains, Mgen = Mgen, 
#                        adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)

# Fit the Peak Over Threshold model
potfit1 <- hmevr::fit_ev_model(data1, model = potmodel,     
                       iter = iter, chains=chains, 
                       Mgen = Mgen, adapt_delta = adapt_delta)

# Fit the GEV model for annual maxima
gevfit1 <- hmevr::fit_ev_model(data1, model = gevmodel, 
                       iter =iter, chains=chains, 
                       Mgen = Mgen, adapt_delta = adapt_delta)

# Compute quantiles for the three extreme value models
# for return times corresponsing to the observed values in maxima1
# and larger than the minimum value trmin selected.
gevq1 = hmevr::comp_quant(gevfit1, maxima1, trmin = trmin)
potq1 = hmevr::comp_quant(potfit1, maxima1, trmin = trmin)
dynq1 = hmevr::comp_quant(dynfit1, maxima1, trmin = trmin)
# staq1 = hmevr::comp_quant(stafit1, maxima1, trmin = trmin)

# now repeat the analysis using the first 50 years for fitting
datasets2 = split_obs_data(df, M_cal = sample_cal_2, 
                           M_val = 270, 
                           Nt = Nt,
                           cross_val = FALSE, 
                           reshuffle = reshuffle_years, 
                           flip_time = FALSE,
                           reshuffle_days = FALSE,
                           decluster = decluster,
                           signif_lim = corr_signif_lim)

data2   = datasets2$datacal$data
maxima2 = datasets2$dataval$max

dynfit2 <- fit_ev_model(data2, model = hmevmodel, 
                        iter = iter, chains=chains,
                        Mgen = Mgen, adapt_delta = adapt_delta, 
                        thresh_hbev = thresh_hbev, priorpar = dgu_prpar)

sim <- rstan::extract(dynfit2$model_fit)


# stafit2 <- fit_ev_model(data2, model = 'wei_sta_bin', 
#                         iter = iter, chains=chains , Mgen = Mgen, 
#                         adapt_delta = adapt_delta, thresh_hbev = thresh_hbev)

potfit2 <- fit_ev_model(data2, model = potmodel, iter =iter, chains=chains,  
                        Mgen = Mgen, adapt_delta = adapt_delta)

gevfit2 <- fit_ev_model(data2, model = gevmodel, iter =iter, chains=chains,
                        Mgen = Mgen, adapt_delta = adapt_delta)

gevq2 = comp_quant(gevfit2, maxima2, trmin = trmin)
potq2 = comp_quant(potfit2, maxima2, trmin = trmin)
dynq2 = comp_quant(dynfit2, maxima2, trmin = trmin)


############################# Plot MCMC Pairs ##################################

mysim <- rstan::extract(dynfit2$model_fit)
mysim <- as.array(dynfit2$model_fit)


mynames = c("mc", "mw", "sc", "sw", "pn")

x_mc = seq(from = 6.5, to = 8.5, length.out = 100)
x_mw = seq(from = 0.65, to = 0.8, length.out = 100)
x_sc = seq(from = 0.2, to = 2, length.out = 100)
x_sw = seq(from = 0.005, to = 0.07, length.out = 100)
x_pn = seq(from = 0.28, to = 0.35, length.out = 100)

d_mc = extraDistr::dinvgamma(x_mc, 10, 79)
d_mw = extraDistr::dinvgamma(x_mw, 10, 7)
d_sc = extraDistr::dinvgamma(x_sc, 10, 19.75)
d_sw = extraDistr::dinvgamma(x_sw, 10, 0.35)
d_pn = dbeta(x_pn, 4, 4)

dfpr <- melt(data.frame(list(mc = x_mc, mw = x_mw, sc = x_sc, sw = x_sw, pn = x_pn)))
colnames(dfpr) = c("Parameter", "x")

# dfprx$what <- 'x'
dfprd <- melt(data.frame(list(mc = d_mc, mw = d_mw, sc = d_sc, sw = d_sw, pn = d_pn)))
colnames(dfprd) = c("Parameter", "density")

dfpr$density = dfprd$density

ggplot()+
geom_line(data=dfpr, aes(x=x, y = density))+
  facet_wrap(~Parameter, scales = 'free')

  
mylabels2 <- as_labeller(c(  mc =   "mu[delta]",
                             mw =      "mu[gamma]",
                             sc =      "sigma[delta]",
                             sw =      "sigma[gamma]",
                             pn =      "lambda"
                                 ), label_parsed)
  
color_scheme_set(scheme = "red")
PL = mcmc_dens_overlay(mysim, pars = mynames,  facet_args = list(labeller = mylabels2))
PL <- PL + geom_line(data=dfpr, aes(x=x, y = density), inherit.aes = FALSE, alpha = 0.6)
PL <- PL + geom_area(data=dfpr, aes(x=x, y = density), inherit.aes = FALSE, alpha = 0.3)
PL <- PL + theme(legend.position = c(0.8, 0.20), 
                 text = element_text(size=20),
                 # plot.margin=unit(c(1,1,1.5,1.2),"cm")
                 plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")
                 )
ggsave(file.path(outplot, 'priors_vs_post.png'), plot=PL, device = 'png',
                       width = 8, height = 5)
# plot(PL)


mylabels <- c( expression(mu[delta]),
                                  expression(mu[gamma]),
                                  expression(sigma[delta]),
                                  expression(sigma[gamma]),
                                  expression(lambda))
mypairs <- pairs(dynfit2$model_fit, pars = c("mc", "mw", "sc", "sw", "pn"),
                      labels = mylabels)
ggsave(file.path(outplot, 'nycp_pairs.ng'), plot=mypairs, device = 'png',
                       width = 9, height = 8)
# plot(mypairs)

################################################################################

########################## PLOT NYCP TIME SERIES ###############################
nyc_acf = acf(df$PRCP, lag.max = 10)
dev.off()
df <- transform(df, REALDATE = as.Date(as.character(DATE), "%Y%m%d"))
pp0 <- ggplot() +
  geom_line(aes(df$REALDATE, df$PRCP))+
  theme_bw()+
  ylim(0, 230)+
  labs(y = "Rainfall accumulation [mm/day]", x= "Year") +
  annotate("text", x=as.Date("1870-01-01"), y=225, label="a)", size = 8)+
theme(
      text = element_text(size=15)
      ) 

pp1 <- ggplot() +
  geom_line(aes(x = res$year, y = res$max)) + 
  # geom_point(aes(x = res$year, y = res$max)) + 
  labs(y = "Annual maximum rainfall [mm/day]", x= "Year") +
  theme_bw()+
  ylim(0, 230)+
  annotate("text", x=1870, y=225, label="b)", size = 8) +
theme(
      text = element_text(size=15)
      ) 


pp2 <- ggplot() +
  theme_bw()+
  labs(y = "Correlation", x= "Time lag [days]") +
  geom_line(aes(x = as.vector(nyc_acf$lag), y = as.vector(nyc_acf$acf)))+
  geom_point(aes(x = as.vector(nyc_acf$lag), y = as.vector(nyc_acf$acf)))+
  geom_line(aes(x = as.vector(nyc_acf$lag), y = rep(0.1, 
                        length(nyc_acf$lag))), lty = 2, color = 'black') + 
  annotate("text", x=0.7, y=0.99, label="c)", size = 8) +
theme(
      text = element_text(size=15)
      ) 


dfxx = df
nobs = length(dfxx$PRCP)
xx = dfxx$PRCP[1:(nobs-1)]
xxp1 = dfxx$PRCP[2:(nobs)]
both_pos = (xx > 0 & xxp1 > 0)
pp3 <- ggplot()+
  theme_bw()+
  labs(y = "x (t + 1)", x= " x(t)") +
  geom_point(aes(x = xx[both_pos], y = xxp1[both_pos]), shape = 1)+
  scale_x_continuous(breaks=c(0.5, 1, 2, 5, 10, 20, 50, 200)) + 
  scale_y_continuous(breaks=c(0.5, 1, 2, 5, 10, 20, 50, 200)) + 
  coord_trans(x="log10", y='log10') +
  annotate("text", x=0.3, y=200, label="d)", size = 8) +
theme(
      text = element_text(size=15)
      ) 
 
nycpfig <- grid.arrange(pp0, pp1, pp2, pp3, nrow = 2)
ggsave(file.path(outplot, 'nycp_time_series.png'), plot=nycpfig, device = 'png',
                       width = 9, height = 8)


# plot quantiles vs Return time
linesize = 0.7
shadealpha = 0.4
p1 <- ggplot() +
geom_line(aes(gevq1$TrvalQ, gevq1$qupper), linetype="dotted",color="red") +
geom_line(aes(gevq1$TrvalQ, gevq1$qlower), linetype="dotted",color="red") +
geom_ribbon(aes(x = gevq1$TrvalQ, ymax = gevq1$qupper, ymin = gevq1$qlower),  
            alpha = shadealpha,  fill = "red") +

geom_line(aes(potq1$TrvalQ, potq1$qupper), linetype="dotted",color="green") +
geom_line(aes(potq1$TrvalQ, potq1$qlower), linetype="dotted",color="green") +
geom_ribbon(aes(x = potq1$TrvalQ, ymax = potq1$qupper, ymin = potq1$qlower),  
            alpha = shadealpha,  fill = "green") +

geom_line(aes(dynq1$TrvalQ, dynq1$qupper), linetype="dotted",color="blue") +
geom_line(aes(dynq1$TrvalQ, dynq1$qlower), linetype="dotted",color="blue") +
geom_ribbon(aes(x = dynq1$TrvalQ, ymax = dynq1$qupper, ymin = dynq1$qlower),  
            alpha = shadealpha,  fill = "blue") +

geom_point(aes( res$Tr, res$Xi),  size=3, color="black", shape=1) +
geom_point(aes( datasets1$datacal$Tr, datasets1$datacal$Xi),  
           size = 3, color = "black", shape = 2) +

geom_line(aes(  gevq1$TrvalQ, gevq1$qmean,color= "red"),  
            linetype="dotdash", size = linesize) +
geom_line(aes(  potq1$TrvalQ, potq1$qmean, color="green"),  
          linetype="twodash", size = linesize) +
geom_line(aes(  dynq1$TrvalQ, dynq1$qmean,color= "blue"),  
          linetype="solid", size = linesize) +
  
scale_color_identity(name = "Model fit",
                   breaks = c("red", "green", "blue"),
                   labels = c("GEV", "POT", "HMEV"),
                   guide = "legend") +

annotate("text", x=2.5, y=450, label="a)", size = 8) +
coord_trans(x="log10", y='log10') +
labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
# xlim(2, 150) +
ylim(50, 500) +
scale_x_continuous(breaks=c(2, 5, 10, 20, 50, 100, 150), limits = c(2, 151)) + 
theme_bw()+
theme(legend.position = c(0.3, 0.82),
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      text = element_text(size=18)
      ) 

p2 <- ggplot() +
geom_line(aes(gevq2$TrvalQ, gevq2$qupper), linetype="dotted",color="red") +
geom_line(aes(gevq2$TrvalQ, gevq2$qlower), linetype="dotted",color="red") +
geom_ribbon(aes(x = gevq2$TrvalQ, ymax = gevq2$qupper, ymin = gevq2$qlower),  
            alpha = shadealpha,  fill = "red") +

geom_line(aes(potq2$TrvalQ, potq2$qupper), linetype="dotted",color="green") +
geom_line(aes(potq2$TrvalQ, potq2$qlower), linetype="dotted",color="green") +
geom_ribbon(aes(x = potq2$TrvalQ, ymax = potq2$qupper, ymin = potq2$qlower),  
            alpha = shadealpha,  fill = "green") +
# 
geom_line(aes(dynq2$TrvalQ, dynq2$qupper), linetype="dotted",color="blue") +
geom_line(aes(dynq2$TrvalQ, dynq2$qlower), linetype="dotted",color="blue") +
geom_ribbon(aes(x = dynq2$TrvalQ, ymax = dynq2$qupper, ymin = dynq2$qlower),  
            alpha = shadealpha,  fill = "blue") +
  
geom_point(aes( res$Tr, res$Xi),  size=3, color="black", shape=1) +
geom_point(aes( datasets2$datacal$Tr, datasets2$datacal$Xi),  
           size = 3, color = "black", shape = 2) +

geom_line(aes(  gevq2$TrvalQ, gevq2$qmean),  
          linetype="dotdash", color="red", size = linesize) +
geom_line(aes(  potq2$TrvalQ, potq2$qmean),  
          linetype="twodash", color="green", size = linesize) +
geom_line(aes(  dynq2$TrvalQ, dynq2$qmean),  
          linetype="solid",color= "blue", size = linesize) +
  
annotate("text", x=2.5, y=450, label="b)", size = 8) +
coord_trans(x="log10", y='log10') +
labs(y = "Quantile [mm/day]", x= "Return Time [years]") +
ylim(50, 500) +
scale_x_continuous(breaks=c(2, 5, 10, 20, 50, 100, 150), limits = c(2, 151))+ 
theme_bw()+
theme(
      text = element_text(size=18)
      ) 

fig <- grid.arrange(p1, p2, ncol=2)
ggsave(file.path(outplot, 'nycp_50_years.png'), plot=fig, device = 'png',
                       width = 9, height = 5)



# ################################
#   model_fit = dynfit1
#   datamat = datasets2$datacal$data
#   ndraws2plot = 100
#   model_sim = rstan::extract(model_fit$model_fit)
#   simdata = hbev_xij_ppd(model_fit)
#   
#   maxima = apply(datamat, 1, max)
#   Mgen = model_fit$Mgen
#   exceedances = pmax(datamat - model_fit$thresh_hbev, 0)
#   excesses = datamat[datamat > model_fit$thresh_hbev] - model_fit$thresh_hbev
#   nyears = dim(exceedances)[1]
#   N = rep(0, nyears)
#   for (i in 1:nyears){
#     samplei = exceedances[i,]
#     N[i] = length(samplei[samplei > 0])
#   }
#         Nplot = N[1:(Mgen)]
#         simvalues = model_sim$Ngen[1:ndraws2plot,]
#         maxval = max(max(simvalues), max(Nplot))
#         minval = min(min(simvalues), min(Nplot))
#         mybreaks = minval:maxval
#         ncounts = length(mybreaks)-1
#         ndraws = dim(model_sim$Ngen)[1]
#         mycounts = matrix(0, nrow = ncounts, ncol = ndraws2plot)
#         myhist_obs = hist(Nplot, breaks = mybreaks, freq = TRUE)
#         for (ix in 1:ndraws2plot){
#           print(ix)
#           # myhist_ix = hist(sim$NrepO[ix, ], breaks = mybreaks, freq = TRUE)
#           myhist_ix = hist(model_sim$Ngen[ix, ], breaks = mybreaks, freq = TRUE)
#           mycounts[,ix ] = myhist_ix$counts
#         }
#         
#         mean_counts = rowMeans(mycounts)
#         upper_quants = apply(mycounts, 1, quantile, probs = 0.75)
#         lower_quants = apply(mycounts, 1, quantile, probs = 0.25)
#         
#         fig2 <- bayesplot::ppc_intervals(y = myhist_obs$counts,
#                                          yrep = t(mycounts),
#                                          size=1,
#                                          prob_outer = 0.90,
#                                          prob = 0.50)
#        plot(fig2) 
#         
        
# plot pdfs of posterior predictive quantities for the second case (50 yrs)
# same function as in hbevr (with few differences)
plot_ppd_pdfs <- function(model_fit, datamat, ndraws2plot = 100){
  " plot posterior predictive distributions for N, maxima and daily data xij
   ndraws2plot -> number of draws to limit at for plotting only
  # Mgen must be equal in size to the observed sample
  datamat -> matrix with obs data (nyears*Nt) "
  
  # test posterior predictive distribution for maxima and ordinary values
  model_sim = rstan::extract(model_fit$model_fit)
  simdata = hbev_xij_ppd(model_fit)
  
  maxima = apply(datamat, 1, max)
  Mgen = model_fit$Mgen
  exceedances = pmax(datamat - model_fit$thresh_hbev, 0)
  excesses = datamat[datamat > model_fit$thresh_hbev] - model_fit$thresh_hbev
  nyears = dim(exceedances)[1]
  N = rep(0, nyears)
  for (i in 1:nyears){
    samplei = exceedances[i,]
    N[i] = length(samplei[samplei > 0])
  }
  fig1 <- ppc_dens_overlay(log(maxima[1:Mgen]),
                           log(simdata$max[1:ndraws2plot,])) + 
          # labs(x=TeX("$\\log \\left( max_i \\left{ x_{i,j} \\right} \\right)$"))
          labs(x=TeX("Block maxima: $\\log{(max_i(x_{i,j}))}$"))
  
  # fig2 <- ppc_dens_overlay(N[1:(Mgen)],model_sim$Ngen[1:ndraws2plot,]) + 
  #         labs(x=TeX("Yearly number of events $n_{j}$"))
  # 
        # FOR N PLOT HISTOGRAM INSTEAD
        Nplot = N[1:(Mgen)]
        simvalues = model_sim$Ngen[1:ndraws2plot,]
        maxval = max(max(simvalues), max(Nplot))
        minval = min(min(simvalues), min(Nplot))
        mybreaks = minval:maxval
        ncounts = length(mybreaks)-1
        ndraws = dim(model_sim$Ngen)[1]
        mycounts = matrix(0, nrow = ncounts, ncol = ndraws2plot)
        myhist_obs = hist(Nplot, breaks = mybreaks, freq = TRUE)
        for (ix in 1:ndraws2plot){
          print(ix)
          # myhist_ix = hist(sim$NrepO[ix, ], breaks = mybreaks, freq = TRUE)
          myhist_ix = hist(model_sim$Ngen[ix, ], breaks = mybreaks, freq = TRUE)
          mycounts[,ix ] = myhist_ix$counts
        }
        
        mean_counts = rowMeans(mycounts)
        upper_quants = apply(mycounts, 1, quantile, probs = 0.75)
        lower_quants = apply(mycounts, 1, quantile, probs = 0.25)
        
        fig2 <- bayesplot::ppc_intervals(y = myhist_obs$counts,
                                         yrep = t(mycounts),
                                         size=1,
                                         prob_outer = 0.90,
                                         prob = 0.50)
  
  fig3 <- ggplot()

  for (s in 1:ndraws2plot){
    sample_s = simdata$xij[s, ]
    wets_s = sample_s[sample_s > 0]
    # print(sum(is.na(log(wets_s))))
    fig3 = fig3 + geom_line( aes_string(log(wets_s)), 
                             stat = 'density', adjust=1/2,
                             alpha = 0.3,
                             # col='lightblue', lwd = 0.2)
                             col='steelblue2', lwd = 0.2)
    
  }
  fig3 = fig3 + geom_line( aes(log(excesses)), 
                         stat = 'density', adjust=1, 
                         col = 'black', lwd=1, alpha = 0.8)
  # print(max(log(excesses)))
  # print(min(log(excesses)))

  # fig3 = fig3 +  coord_trans(x="log10")
  fig3 = fig3 + scale_x_continuous(limits = c(-4, 6))
  fig3 = fig3 + labs( y = "",  x = TeX("Daily events $\\log{(x_{i,j}) }$"))
  fig3 = fig3 + theme_bw() + theme(panel.border = element_blank(), 
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), 
                                   axis.line = element_line(colour = "black"),
                                   axis.text.y=element_blank(), 
                                   text = element_text(size=18),
                                   axis.ticks=element_blank())
  fig2 = fig2 + theme_bw() + theme(panel.border = element_blank(), 
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), 
                                   axis.line = element_line(colour = "black"),
                                   axis.text.y=element_blank(), 
                                   text = element_text(size=18),
                                   axis.ticks=element_blank())
  fig1 = fig1 + theme_bw() + theme(panel.border = element_blank(), 
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), 
                                   axis.line = element_line(colour = "black"),
                                   axis.text.y=element_blank(), 
                                   text = element_text(size=18),
                                   axis.ticks=element_blank())
    
    fig1 = fig1 + annotate("text", x=3.45, y= 1.8*0.85, label="a)",size = 10)
    fig2 = fig2 + annotate("text", x=10, y= 4.2*0.85, label="b)", size = 10)
    fig3 = fig3 + annotate("text", x=-3.6, y=0.89*0.3, label="c)", size = 10) 
    fig1 = fig1 + ylim(0, 1.8)
    fig2 = fig2 + ylim(0, 4.2)
    fig3 = fig3 + ylim(0, 0.3)
  
  fig <- grid.arrange(fig1, fig2, fig3,ncol=3)
  return(fig)
}

# mat_events = datasets2$datacal$data
mat_events = pmax(datasets2$datacal$data-thresh_hbev, 0.0)

color_scheme_set(scheme = "blue")
fig2 <- plot_ppd_pdfs(dynfit2, mat_events, ndraws2plot = 100)
plot(fig2)
ggsave(file.path(outplot, 'nycp_ppdf.png'), plot=fig2, device = 'png',
                       width = 12, height = 6.5)
# dim(dataset2$datacal$data)


###################################################

# DO PRIOR PREDICTIVE CHEKCS:

###################################################

print(dynfit1$prior$prior_par)
print(dynfit1$prior$prior_name)

# DYN: GENERATE PRIOR PARAMETERS::
# draw samples from PRIOR distributions
ndrawspr = 400 # number of annual maxima to draw
# pnpr <- dynfit1$prior$prior_rng$pn0prior[1:ndrawspr] # up to 4000
# scpr <- dynfit1$prior$prior_rng$sc0prior[1:ndrawspr] # up to 4000
# mcpr <- dynfit1$prior$prior_rng$mc0prior[1:ndrawspr] # up to 4000
# swpr <- dynfit1$prior$prior_rng$sw0prior[1:ndrawspr] # up to 4000
# mwpr <- dynfit1$prior$prior_rng$mw0prior[1:ndrawspr] # up to 4000
pnpr <- rbeta(ndrawspr, 4.0, 4.0)
scpr <- rinvgamma(ndrawspr, 10.0, 19.74)
mcpr <- rinvgamma(ndrawspr, 10.0, 79.00)
swpr <- rinvgamma(ndrawspr, 10.0, 0.35)
mwpr <- rinvgamma(ndrawspr, 10.0, 7.0)
# ndrawspr = length(scpr) # 4000 by default

awpr = (mwpr/swpr)^2
bwpr =  mwpr/(swpr)^2
acpr = (mcpr/scpr)^2
bcpr =  mcpr/(scpr)^2

  # cprs <- rgamma(1,  acpr[s], bcpr[s])
  # wprs <- rgamma(1,  awpr[s], bwpr[s])
  # nprs <- rbinom(1, Nt, pnpr[s])

# ndrawspr = 1000 # number of random draws from the prior 
# POT: draw samples:
gevfit1$prior$pr_mu # normal
gevfit1$prior$pr_psi # gamma
gevfit1$prior$pr_k # normal
gev_prmu = rnorm(ndrawspr, gevfit1$prior$pr_mu[1], gevfit1$prior$pr_mu[2])
gev_prpsi = rgamma(ndrawspr, gevfit1$prior$pr_psi[1], gevfit1$prior$pr_psi[2])
gev_prk = rnorm(ndrawspr, gevfit1$prior$pr_k[1], gevfit1$prior$pr_k[2])
# 

potfit1$prior$pr_lambda # gamma
potfit1$prior$pr_sigma # gamma
potfit1$prior$pr_k # normal
pot_prlambda = rgamma(ndrawspr, potfit1$prior$pr_lambda[1], potfit1$prior$pr_lambda[2])
pot_prsigma = rgamma( ndrawspr, potfit1$prior$pr_sigma[1], potfit1$prior$pr_sigma[2])
pot_prk = rnorm(      ndrawspr, potfit1$prior$pr_k[1], potfit1$prior$pr_k[2])
ymin = potfit1$thresh_pot
pot_prmu = ymin + pot_prsigma/pot_prk*(pot_prlambda^(pot_prk) - 1)
pot_prpsi  = pot_prsigma*pot_prlambda^pot_prk

plot(density(gev_prk))
plot(density(gev_prpsi))
plot(density(gev_prmu))

potq_max = rep(0.0,ndrawspr)
gevq_max = rep(0.0,ndrawspr)
dynq_max = rep(0.0,ndrawspr)
for (s in 1:ndrawspr){
  # potq_max[s] = evd::rgev(1, loc=pot_prmu[s], scale=pot_prpsi[s], shape=pot_prk[s])
  # gevq_max[s] = evd::rgev(1, loc=gev_prmu[s], scale=gev_prpsi[s], shape=gev_prk[s])
  potq_max[s] = extraDistr::rgev(1, mu=pot_prmu[s], sigma=pot_prpsi[s], xi=pot_prk[s])
  gevq_max[s] = extraDistr::rgev(1, mu=gev_prmu[s], sigma=gev_prpsi[s], xi=gev_prk[s])
  cprs <- rgamma(1,  acpr[s], bcpr[s])
  wprs <- rgamma(1,  awpr[s], bwpr[s])
  nprs <- rbinom(1, Nt, pnpr[s])
  wets = rweibull(nprs, wprs, cprs)
  dynq_max[s] = max(wets)
}


dfq0 = melt(as.data.frame(list(Observed=maxima1))) 
dfq = melt(as.data.frame(list(POT=potq_max, GEV=gevq_max, HMEV=dynq_max))) 
# dfq = melt(as.data.frame(list(POT=potq_max, GEV=gevq_max)))
# dfq = melt(as.data.frame(list(Observed=maxima1, POT=potq_max, GEV=gevq_max, HMEV=dynq_max))) 


priorfig <- ggplot(dfq) +
  geom_density(aes(x=value, color=variable), size=0.7) + 
  geom_density(data=dfq0, aes(x=value, linetype=variable), color='black', size=0.7) + 
  theme_bw() + xlim(0, 400) + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=c("green", "red", "blue"))
# scale_color_manual(values=c("black", "green", "red", "blue"))
plot(priorfig)
ggsave(file.path(outplot, sprintf('priorpredcheck_%s.png', stat_id)), plot=priorfig, device = 'png',
       width = 3, height = 3)


