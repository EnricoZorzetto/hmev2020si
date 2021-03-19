
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)
library(viridis)
# Get the world polygon and extract the USA
library(maps)
library(stringr)
library(mapdata)
library(latex2exp)
library(showtext) # needed to add plot labels in rstudio - conda - pop_os
showtext_auto()

# choose one of the following datasets [results] to plot:
# dset = 'kfold_37'
# dset = 'stats_479'
# dset = 'kfold_479'
# dset = 'kfold_479_1_nodec'
dset = 'kfold_479_5_nodec'
# dset = 'stats_1113'
# dset = 'stats_37'
# dset = 'kfold_37'
# dset = 'stats_21'
# dset = 'kfold_21'

minlength = 100 # min length of stations included in the analysis

# setwd( file.path('~','Projects','hbev','codes'))

# select no thresh, 1mm thresh, or declust
# outdata = file.path('..', 'output', 'output_data_0', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_0dec', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_1', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_backup_synth', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_last_priors', sprintf('output_%s', dset))
# resdir = file.path('..', 'output')
resdir = file.path('..', 'output_5_nodec')


outdata = file.path(resdir, 'output_data', sprintf('output_%s', dset))
outplot = file.path(resdir, 'outplot')
dir.create(outplot, showWarnings = FALSE)

# to save many plots in a separate folder::
# outplotStats = file.path('..', 'output', 'outplot', sprintf('plots_%s', dset))
# dir.create(outplotStats, showWarnings = FALSE)



# read the results from data analysis on CONUS stations
files <- list.files(outdata)
nfiles <- length(files)


datalist <- list()
# read in sequence and concat data
for (i in 1:nfiles) {
  dat <-  read.table( file.path(outdata, files[i]), sep = ',', 
                    header = TRUE, row.names = 1)
  datalist[[i]] <- dat # add it to your list
}
dfr <- data.table::rbindlist(datalist)

# change names for ease:
dfr$gof <- sapply(dfr$gof, function(x) gsub("_", "", x))

# drop two variables we are not using:
dfr <- subset(dfr, dfr$gof != 'ploo' & dfr$gof != 'elpdloo')

# drop other models we are not using:

modnames = c("gev", "pot_ppp", "wei_dgu_bin")
# modnames = c("gev", "pot_ppp", "wei_dgu_bin")
# modnames = c("gev", "pot_ppp", "wei_sta_bin")
# modnames = c("gev", "wei_dyn_bin", "wei_sta_bin")

dfr <- subset(dfr, dfr$gof != 'ploo' & dfr$model %in% modnames)



# dfr$gof[dfr$gof == "trmin_quant"] <- "trminQuant"
# dfr$gof[dfr$gof == "elpd_loo"] <- "elpdLoo"
# dfr$gof[dfr$gof == "p_loo"] <- "pLoo"
# dfr$gof[dfr$gof == "elpd_waic2"] <- "elpdWaic2"
# dfr$gof[dfr$gof == "p_waic2"] <- "pWaic2"



# check if there are any problematic station fit:
# should be none!
# dfp = unique(dfr$ID[(dfr$model == 'gev' & dfr$ssize > 20 & dfr$score > 100000)])
# dfp = subset(dfr, dfr$gof != 'trmin_quant' & dfr$gof != 'p_waic2' 
#              & dfr$gof != 'elpd_waic2'& dfr$score > 1000)


# rename models and test variables (GEV / POT / HBEV), (IS - OS)
    # rename sample size for decent labelling
    # dfr$model <- as.factor(dfr$model)

# dfr$model <- sapply(dfr$model, is.factor)
# dfr$test <- sapply(dfr$test, is.factor)

# dfr$model[dfr$model == 'gev'] <- 'GEV'
# dfr$model[dfr$model == 'pot_ppp'] <- 'POT'
# dfr$model[dfr$model == 'wei_dgu_bin'] <- 'HBEV'
# 
# dfr$test[dfr$test == 'ss'] <- 'IS'
# dfr$test[dfr$test == 'cv'] <- 'OS'
# # 
dfr$test <- factor(dfr$test, levels = c("ss", "cv"),
                labels = c("'In sample'", "'Out of sample'"))
dfr$model <- factor(dfr$model, levels = modnames, 
                    
                labels = c("GEV", "POT", "HMEV"))


# to save many plots::
# outplotStats = file.path('..', 'output', 'outplot', sprintf('plots_%s', dset))
# dir.create(outplotStats, showWarnings = FALSE)


# plot boxplot with measures of performance for the three models
BOXPLOTS = list()
mygofs0 = unique(dfr$gof)
plgofs = c("lppd", "lpml", "fse", "mwidth", "mbias")
nplgofs = length(plgofs)
for (i in 1:nplgofs){
   mygof = plgofs[i] 
    dfpl = subset(dfr , dfr$gof == mygof)
    
    # rename sample size for decent labelling
    dfpl$ssize <- as.factor(dfpl$ssize)
ssizevals = unique(dfpl$ssize)
nssizes = length(ssizevals)
for (i in 1:nssizes){
    levels(dfpl$ssize)[levels(dfpl$ssize)==ssizevals[i]
                       ] <- sprintf("M[train] == %s", ssizevals[i])
}
    
    gi <- ggplot(dfpl, aes(x=model, y=score, fill=model)) +
               geom_boxplot() +
               theme_bw() + 
               theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               text = element_text(size=15),
               strip.background =element_rect(fill="white"),
               legend.position = 'bottom') +
               # geom_jitter(width=0.25, alpha=0.5) +
               # ylim(c(0, 20)) +
               # ylim(c(3, 10)) +
               ylab(mygof) + 
               facet_grid(test~ssize, scales='free', labeller = label_parsed) +
      
      # scale_color_manual(values=c("red", "green", "blue")) +
      
        # scale_color_identity(name = "Model fit",
        #                breaks = c("gev", "pot_ppp", "wei_dgu_bin"),
        #                labels = c("GEV", "POT", "HBEV"),
        #                guide = "legend") +
      
               ggsave(file.path(outplot, sprintf('%s_mygof_%s.png', 
                         dset, mygof)), width = 8, height = 5)   
    BOXPLOTS[[mygof]] <- gi
        
}



# read position (LAT, LON) of all stations/ runs
names(dfr)[names(dfr) == 'station'] <- 'ID'
# load stations positions and subset them
posfile = file.path('..', 'data', 'Data_GHCN_Daily', 'list_ushcn_stats.csv')
dfpos_all = read.table(posfile, sep=',', header = TRUE)
dfpos = subset(dfpos_all, dfpos_all$NYEARS_36 >= minlength)
print(dim(dfpos))
dfpos <- dfpos[c("LATITUDE", "LONGITUDE", "ID")]
dfr <- merge(dfpos, dfr, by = c("ID"))
df3 <- reshape2::dcast(dfr, ID + LATITUDE + LONGITUDE +  numj 
                + gof + ssize + test ~ model, value.var = 'score')

# select only the models I want to compare
# dfval = df3[, c("wei_dgu_bin", "pot_ppp", "gev")]
dfval = df3[, c("HMEV", "POT", "GEV")]
# add to df3 the best among these 3 models only
# BEST counts only for FSE, LPPD, LPML (*-1/m)
df3$best = colnames(dfval)[max.col(-dfval, ties.method="first")] #minimum = best

################################################################################
# now compute the AVERAGE best model over the 5 generations
################################################################################
df_sh0 <- reshape2::dcast(dfr, ID + LATITUDE + LONGITUDE + numj ~ 
                           model + test + ssize + gof, value.var = 'score')
df_sh <- subset(df_sh0, select = -c(numj, ID))
df_sh_ave <- aggregate(df_sh, by = list(df_sh0$ID), FUN = mean , na.rm=FALSE, na.action=NULL)
# df_sh_ave <- aggregate(df_sh, by = list(df_sh$ID), mean)
# df_sh_ave <- subset(df_sh_ave, select = -c(ID))
allcols <- names(df_sh_ave)[4:length(names(df_sh_ave))]
names(df_sh_ave)[names(df_sh_ave) == 'Group.1'] <- 'ID'
df_ave0 <- reshape2::melt(df_sh_ave, id = c("ID", "LATITUDE", "LONGITUDE"),
                         measure = allcols)
csplit <-  reshape2::colsplit(df_ave0[, 4], "_", 
                              c("model", "test", "ssize", "gof"))
df_ave <- cbind(df_ave0[1:3], csplit, df_ave0[5])

# now do to the average the same operation (find optimum)
df3ave <- reshape2::dcast(df_ave, ID + LATITUDE + LONGITUDE 
                + gof + ssize + test ~ model, value.var = 'value')
dfvalave = df3ave[, c("HMEV", "POT", "GEV")]
df3ave$best = colnames(dfvalave)[max.col(-dfvalave, ties.method="first")] 
################################################################################

# modelfrac = 'wei_dgu_bin' # main model I want to compute the success rate of

# bestdf = expand.grid(test = unique(df3$test), gof = unique(df3$gof),
#                      ssize = unique(df3$ssize), value = NA)


# compute the fraction of runs in which each model is the best
MYSSIZE = unique(df3$ssize)
# MYMODEL = c("wei_dgu_bin", "pot_ppp", "gev")
MYMODEL = c("HMEV", "POT", "GEV")
MYTEST = unique(df3$test)
MYGOF = c( 'lppd', 'lpml', 'fse') # only these for mapping
# MYGOF = unique(df3$gof) # only these for mapping
bestdf = expand.grid(test = unique(df3$test), gof = MYGOF,
                     ssize = unique(df3$ssize), model = MYMODEL, value = NA)

for (i in 1:length(MYSSIZE)){
  for (j in 1:length(MYTEST)){
    for (k in 1:length(MYGOF)){

      myssize <- MYSSIZE[i]
      mytest <- MYTEST[j]
      mygof <- MYGOF[k]
      
      dfijk = subset(df3, df3$ssize == MYSSIZE[i] &
                          df3$test == MYTEST[j] &
                          df3$gof == MYGOF[k])
      
      freq <- table(dfijk$best)
      freq <- freq/sum(freq)
      
      for (l in 1:length(MYMODEL)){
        
        mymodel <- MYMODEL[l]
        bestdf$value[(  bestdf$ssize == myssize 
                      & bestdf$gof == mygof 
                      & bestdf$model == mymodel 
                      & bestdf$test == mytest)] <- freq[mymodel]
      }
    } 
  }
}

# df_fse = subset(df3, df3$gof == 'fse')
df_fse = subset(df3ave, df3ave$gof == 'fse')
# instead use the average all values with the same station ID:
# agg = aggregate(df_fse,
#                 by = list(df_fse$ID, df_fse$ssize, df_fse$test),
#                 FUN = mean)
# names(df_fse)[names(df_fse)=="best"] <- "Best Model"
    df_fse$ssize <- as.factor(df_fse$ssize)
ssizevals = unique(df_fse$ssize)
nssizes = length(ssizevals)
for (i in 1:nssizes){
    levels(df_fse$ssize)[levels(df_fse$ssize)==ssizevals[i]
                       ] <- sprintf("M[train] == %s", ssizevals[i])
}

  usa <- map_data("usa") 
ggplot(df_fse) +
      # ggtitle( sprintf("%s sample size=%s, %s ", MYTEST[j], MYSSIZE[i], MYGOF[k])) +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill="grey", alpha=0.6) +
    # coord_quickmap() +  
    # geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=best, shape=best)) +
    geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=best), alpha = 0.9, size = 0.6) +
  # ylab("Sample size")+
    labs(color = "Model")+
   
      scale_color_manual(values=c("red", "blue", "green")) +
  
    # annotate('text', x=-115, y=26, label=sprintf('bm = %s', round(freq[modelfrac], 2))) +
    # scale_fill_discrete(name = "best model")   + 
  #   su <- signup('enrico89', 'enrico.zorzetto@duke.edu', save = TRUE)
    # scale_size_continuous(range=c(1,12)) +
    # scale_color_viridis(trans="log") +
    theme_bw()+
                   theme(
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               text = element_text(size=15),
               strip.background =element_rect(fill="white"),
               legend.position = 'bottom') +
  # labs(fill = "Best Model") +
# scale_fill_discrete(name = "Best Model")
               # theme(legend.position = 'bottom') +
    # coord_map()  +
  facet_grid(ssize~test, labeller = label_parsed)+
    ggsave(file.path(outplot, sprintf('fse_maps_%s.png', dset)),
          width = 6, height = 8)

df_lppd = subset(df3ave, df3ave$gof == 'lppd')
# instead use the average all values with the same station ID:
# agg = aggregate(df_fse,
#                 by = list(df_fse$ID, df_fse$ssize, df_fse$test),
#                 FUN = mean)
# names(df_fse)[names(df_fse)=="best"] <- "Best Model"
    df_lppd$ssize <- as.factor(df_lppd$ssize)
ssizevals = unique(df_lppd$ssize)
nssizes = length(ssizevals)
for (i in 1:nssizes){
    levels(df_lppd$ssize)[levels(df_lppd$ssize)==ssizevals[i]
                       ] <- sprintf("M[train] == %s", ssizevals[i])
}

  # usa <- map_data("usa") 
ggplot(df_lppd) +
      # ggtitle( sprintf("%s sample size=%s, %s ", MYTEST[j], MYSSIZE[i], MYGOF[k])) +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill="grey", alpha=0.6) +
    # coord_quickmap() +  
    # geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=best, shape=best)) +
    geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=best), alpha = 0.9, size = 0.6) +
  # ylab("Sample size")+
    labs(color = "Model")+
   
      scale_color_manual(values=c("red", "blue", "green")) +
  
    # annotate('text', x=-115, y=26, label=sprintf('bm = %s', round(freq[modelfrac], 2))) +
    # scale_fill_discrete(name = "best model")   + 
  #   su <- signup('enrico89', 'enrico.zorzetto@duke.edu', save = TRUE)
    # scale_size_continuous(range=c(1,12)) +
    # scale_color_viridis(trans="log") +
    theme_bw()+
                   theme(
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               text = element_text(size=15),
               strip.background =element_rect(fill="white"),
               legend.position = 'bottom') +
  # labs(fill = "Best Model") +
# scale_fill_discrete(name = "Best Model")
               # theme(legend.position = 'bottom') +
    # coord_map()  +
  facet_grid(ssize~test, labeller = label_parsed)+
    ggsave(file.path(outplot, sprintf('lppd_maps_%s.png', dset)),
          width = 6, height = 8)
############################ checked until here - add only plots ###############

################################################################################

################################################################################


  # plot fractions of stations where HBEV wins as a function of sample size
  # first let's change some names to make the plot prettier
  bestdf2 = subset(bestdf, gof %in% c( "lppd", "fse"))
  # bestdf2 = subset(bestdf, gof %in% c("lpml", "lppd", "fse"))
  # bestdf2 = subset(bestdf2,(gof != 'lpml' | test != "'Out of sample'") )
  names(bestdf2)[names(bestdf2) == 'gof'] <- 'Measure'
  
  # optionally to remove the lpml in cross validation, uncomment the next line
  # bestdf2 <- subset(bestdf2,  (bestdf2$Measure != 'lpml' )|( bestdf2$test != 'cv') )
  # bestdf2$test <- factor(bestdf2$test, levels = c("ss", "cv"),
  #                 labels = c("IS", "OS"))
  figfrac <- ggplot(bestdf2) + 
    # geom_line(aes(x = ssize, y = value, color = model, lty = model), lwd =1.4 )+
    geom_bar(aes(x = ssize, y = value, fill = model), alpha = 0.6, position="fill", stat="identity")+
    ylim(0, 1)+
    labs(x = 'Sample size [years]', y = 'Fraction of stations')+
    theme_bw() + 
    scale_fill_manual(values=c("blue", "green", "red")) +
  # scale_color_identity(name = "Measure")+
       # facet_wrap(~test~Measure, scales='free')
               theme(legend.position = 'bottom',
               text = element_text(size=15),
               strip.background =element_rect(fill="white"),
                     ) +
       facet_grid(test~Measure, scales='free', labeller = label_parsed)
  figfrac
    ggsave(file.path(outplot, sprintf('bestm_%s_fraction_stations.png', dset)), 
           plot = figfrac, width = 6, height = 5)
    
    
  #     bestdf2 = subset(bestdf, gof %in% c("lpml", "lppd", "fse"))
  # bestdf2 = subset(bestdf2,(gof != 'lpml' | test != "'Out of sample'") )
  # names(bestdf2)[names(bestdf2) == 'gof'] <- 'Measure'
  # 
  # # optionally to remove the lpml in cross validation, uncomment the next line
  # # bestdf2 <- subset(bestdf2,  (bestdf2$Measure != 'lpml' )|( bestdf2$test != 'cv') )
  # # bestdf2$test <- factor(bestdf2$test, levels = c("ss", "cv"),
  # #                 labels = c("IS", "OS"))
  # figfrac <- ggplot(bestdf2) + 
  #   geom_line(aes(x = ssize, y = value, color = model, lty = model), lwd =1.4 )+
  #   ylim(0, 1)+
  #   labs(x = 'Sample size [years]', y = 'Fraction of stations')+
  #   theme_bw() + 
  #   scale_color_manual(values=c("blue", "green", "red")) +
  # # scale_color_identity(name = "Measure")+
  #      # facet_wrap(~test~Measure, scales='free')
  #              theme(legend.position = c(0.8, 0.2),
  #              text = element_text(size=15),
  #              strip.background =element_rect(fill="white"),
  #                    ) +
  #      facet_wrap(test~Measure, scales='free', labeller = label_parsed)
  # figfrac
  #   ggsave(file.path(outplot, sprintf('bestm_%s_fraction_stations_2.png', dset)), 
  #          plot = figfrac, width = 8, height = 5)
    
    
# compute and plot ratio of in sample to out of sample LPPD
    
df4 <- reshape2::dcast(dfr, ID + LATITUDE + LONGITUDE +  numj 
                + test + ssize + model ~ gof, value.var = 'score')   

# df4$ratio <- (df4$ss-df4$cv)/df4$cv
df4$ratio <- (df4$lpml - df4$lppd)*df4$ssize

df4 <- subset(df4, df4$test == "'In sample'")

df4$ssize <- as.factor(df4$ssize)
ssizevals = unique(df4$ssize)
nssizes = length(ssizevals)
for (i in 1:nssizes){
    levels(df4$ssize)[levels(df4$ssize)==ssizevals[i]] <- sprintf("M[train] == %s", ssizevals[i])
}
    
ggplot(df4)+
  geom_boxplot(aes(y=ratio, fill = model))+
  theme_bw()+
    # scale_color_manual(values=c("blue", "green", "red")) +
  # scale_y_continuous(trans='log') + 
  # facet_wrap(~ssize, scales='free')
  # ggtitle("Effective number of parameters")+
  scale_y_continuous(trans='log10', 
                     breaks=c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)) + 
                 theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
                 text = element_text(size=15),
       strip.background =element_rect(fill="white"),
       plot.title = element_text(hjust = 0.5),
               legend.position = 'bottom') +
               # geom_jitter(width=0.25, alpha=0.5) +
               # ylim(c(0, 20)) +
               # ylim(c(3, 10)) +
               # ylab(TeX("$p_{lpml} = \\left( lpml - lppd \\right) \\cdot J$")) + 
               ylab(TeX("Effective number of parameters")) + 
  facet_grid(~ssize, labeller = label_parsed)+
                 ggsave(file.path(outplot, sprintf('eff_num_par_%s.png', 
                         dset)), width = 8, height = 5)   


df5 <- subset(dfr, dfr$test == "'In sample'" & dfr$gof == 'pwaic2')
df5$ssize <- as.factor(df5$ssize)
ssizevals = unique(df5$ssize)
nssizes = length(ssizevals)
for (i in 1:nssizes){
    levels(df5$ssize)[levels(df5$ssize)==ssizevals[i]] <- sprintf("M[train] == %s", ssizevals[i])
}
ggplot(df5)+
  geom_boxplot(aes(y=score, fill = model))+
  theme_bw()+
    # scale_color_manual(values=c("blue", "green", "red")) +
  # scale_y_continuous(trans='log') +
    scale_y_continuous(trans='log10', 
                     breaks=c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)) + 
  
                 theme(axis.title.x=element_blank(),
                      text = element_text(size=15),
       strip.background =element_rect(fill="white"),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               legend.position = 'bottom') +
  # ggtitle("Sample size")+
  # ggtitle("Effective number of parameters")+
              # theme(legend.position="bottom")
               # geom_jitter(width=0.25, alpha=0.5) +
               # ylim(c(0, 20)) +
               # ylim(c(3, 10)) +
               # ylab('Effective number of parameters [waic2]') + 
               ylab(TeX("Effective number of parameters $p_{waic2}$")) + 
  facet_grid(~ssize, labeller = label_parsed)+
               ggsave(file.path(outplot, sprintf('eff_num_par_waic2_%s.png', 
                         dset)), width = 8, height = 5)   

# df_quantG <- subset(dfr, dfr$model == 'GEV' & dfr$gof == 'trmaxquant' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
# df_widthG <- subset(dfr, dfr$model == 'GEV' & dfr$gof == 'trmaxwidth' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
# df_widthG$normwidth = df_width$score /df_quant$score
# 
# mean(df_widthG$normwidth)


# plot map of HBEV quantiles over the CONUS
# sub df_ave instead of dfr
################################################################################
l1 <- expression('q[50]')
usa <- map_data("usa")
df_quant <- subset(df_ave, df_ave$model == 'HMEV' & df_ave$gof == 'trmaxquant' 
                   & df_ave$ssize == 50 & df_ave$test == "'In sample'")
df_width <- subset(df_ave, df_ave$model == 'HMEV' & df_ave$gof == 'trmaxwidth' 
                   & df_ave$ssize == 50 & df_ave$test == "'In sample'")
# df_quant <- subset(dfr, dfr$model == 'HMEV' & dfr$gof == 'trmaxquant' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
# df_width <- subset(dfr, dfr$model == 'HMEV' & dfr$gof == 'trmaxwidth' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
df_quant$Uncertainty = df_width$value /df_quant$value
ggplot(df_quant) +
    geom_polygon(data = usa, aes(x=long, y = lat), fill="grey", alpha=0.9) +
    geom_point(aes(x=LONGITUDE, y=LATITUDE,
                   color=value, size = Uncertainty), alpha = 0.9) +
    # labs(color = "Return level 'q[50]' [mm]",)+
    # labs(color = l1, labeller = label_parsed)+
    # scale_color_continuous(name = l1)   +
    scale_color_viridis(name = expression(paste(q[50], '[mm]')))   +
    # scale_size_continuous(name = expression(paste(Delta, q[50], '/' , q[50])))   +
    scale_size_continuous(guide=FALSE, range = c(0.01, 2))   +
    # scale_color_continuous(name = expression(q[50]))   +
    xlab('Longitude')+
    ylab('Latitude')+
    # scale_color_viridis()+
    theme_bw()+
    # theme(legend.position = 'bottom') +
    theme(legend.position = c(0.18, 0.1), legend.direction = "horizontal",
          legend.background = element_rect(fill=alpha(0.4)))+
    ggsave(file.path(outplot, sprintf('quantiles_maps_%s.png', dset)),
          width = 6.5, height = 4)
################################################################################



l1 <- expression('q[50]')
usa <- map_data("usa")
df_quant <- subset(df_ave, df_ave$model == 'HMEV' & df_ave$gof == 'mbias' 
                   & df_ave$ssize == 50 & df_ave$test == "'In sample'")
df_width <- subset(df_ave, df_ave$model == 'HMEV' & df_ave$gof == 'fse' 
                   & df_ave$ssize == 50 & df_ave$test == "'In sample'")
# df_quant <- subset(dfr, dfr$model == 'HMEV' & dfr$gof == 'trmaxquant' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
# df_width <- subset(dfr, dfr$model == 'HMEV' & dfr$gof == 'trmaxwidth' 
#                    & dfr$ssize == 50 & dfr$test == "'In sample'")
df_quant$Uncertainty = df_width$value /df_quant$value
ggplot(df_quant) +
    geom_polygon(data = usa, aes(x=long, y = lat), fill="grey", alpha=0.9) +
    geom_point(aes(x=LONGITUDE, y=LATITUDE,
                   color=value, size = Uncertainty), alpha = 0.9) +
    # labs(color = "Return level 'q[50]' [mm]",)+
    # labs(color = l1, labeller = label_parsed)+
    # scale_color_continuous(name = l1)   +
    scale_color_viridis(name = expression(paste(q[50], '[mm]')))   +
    # scale_size_continuous(name = expression(paste(Delta, q[50], '/' , q[50])))   +
    scale_size_continuous(guide=FALSE, range = c(0.01, 2))   +
    # scale_color_continuous(name = expression(q[50]))   +
    xlab('Longitude')+
    ylab('Latitude')+
    # scale_color_viridis()+
    theme_bw()+
    # theme(legend.position = 'bottom') +
    theme(legend.position = c(0.18, 0.1), legend.direction = "horizontal",
          legend.background = element_rect(fill=alpha(0.4)))+
    ggsave(file.path(outplot, sprintf('mbias_maps_%s.png', dset)),
          width = 6.5, height = 4)

# 
# # plot map of HBEV quantiles over the CONUS
# usa <- map_data("usa")
# # df_quant <- subset(dfr, dfr$model == 'HMEV' & dfr$gof == 'trmaxwidth' 
# #                    & dfr$ssize == 50 & dfr$test == "'In sample'")
#   # names(df_quant)[names(df_quant) == 'score'] <- 'Return_level'
#   # names(df_quant)[names(df_quant) == 'lat'] <- 'Latitude'
#   # names(df_quant)[names(df_quant) == 'lon'] <- 'Longitude'
# ggplot(df_width) +
#       # ggtitle( sprintf("%s sample size=%s, %s ", MYTEST[j], MYSSIZE[i], MYGOF[k])) +
#     # geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill="grey", alpha=0.4) +
#     geom_polygon(data = usa, aes(x=long, y = lat), fill="grey", alpha=0.4) +
#     # coord_quickmap() +  
#     # geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=best, shape=best)) +
#     geom_point(aes(x=LONGITUDE, y=LATITUDE,  color=normwidth), alpha = 0.4) +
#     labs(color = "Normalized return Level \n uncertainty")+
#     xlab('Longitude')+
#     ylab('Latitude')+
#     scale_color_viridis()+
#   # ylab("Sample size")+
#    
#       # scale_color_manual(values=c("red", "blue", "green")) +
#   
#     # annotate('text', x=-115, y=26, label=sprintf('bm = %s', round(freq[modelfrac], 2))) +
#     # scale_fill_discrete(name = "best model")   + 
#   #   su <- signup('enrico89', 'enrico.zorzetto@duke.edu', save = TRUE)
#     # scale_size_continuous(range=c(1,12)) +
#     # scale_color_viridis(trans="log") +
#     theme_bw()+
#                #     theme(
#                # axis.title.x=element_blank(),
#                # axis.text.x=element_blank(),
#                # axis.ticks.x=element_blank(),
#                # axis.title.y=element_blank(),
#                # axis.text.y=element_blank(),
#                # axis.ticks.y=element_blank(),
#                # panel.grid.major = element_blank(),
#                # panel.grid.minor = element_blank(),
#                # legend.position = 'bottom') +
#   # labs(fill = "Best Model") +
# # scale_fill_discrete(name = "Best Model")
#                # theme(legend.position = 'bottom') +
#     # coord_map()  +
#   # facet_grid(ssize~test)+
#     ggsave(file.path(outplot, sprintf('widths_maps_%s.png', dset)),
#           width = 6, height = 3)