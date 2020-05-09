
library(ggplot2)
library(data.table)
library(plyr)
# library(latex2exp)

# first pull output sync from the cluster
# setwd( file.path('~','Projects','hbev','codes'))
# setwd( file.path('~','Projects','hbev','codes'))
outdata = file.path('..', 'output', 'output_data', 'output_synth')

outplot = file.path('..', 'output', 'outplot')
dir.create(outplot, showWarnings = FALSE)
files = list.files(outdata)
nfiles = length(files)

# read in sequence and concat data
  datalist = list()
  for (i in 1:nfiles) {
    dat = read.table( file.path(outdata, files[i]), sep = ',', 
                      header = TRUE, row.names = 1)
    datalist[[i]] <- dat # add it to your list
  }
  dfr0 <- data.table::rbindlist(datalist)
  dfr = dfr0

  
dfr <- subset(dfr, dfr$ssize %in% c(20 ,50) &
                   dfr$gof %in% c("fse", "mbias", "mwidth") &
                   dfr$model %in% c("gev", "pot_ppp", "wei_dgu_bin"))

# dfr <- subset(dfr, dfr$value < 20000)

# # Modify the data: change names for PLOT LABELLING purposes

dfr$test <- factor(dfr$test, levels = c("ss", "cv"),
labels = c( "'In sample'", "'Out of sample'"))

dfr$model <- factor(dfr$model, levels = c("gev", "pot_ppp", "wei_dgu_bin"),
                  labels = c("GEV", "POT", "HMEV"))
#
dfr$spec <- factor(dfr$spec, levels = c("gam", "wei", "wei_dgu", "gpd"),
                  labels = c("GAM", "WEI", 'WEI[G]',  "GP"))

levelsgof = unique(dfr$gof)

# dfr$gof2 <- factor(dfr$gof, levels = c("elpd_loo", "elpd_waic2", "fse",  "lpml",
#                 "lppd", "mbias", "mwidth", "p_loo", "p_waic2"),
#                 labels = c("elpd_loo", "elpd_waic2", "FSE",  "lpml",
#                 "lppd", "b[q]", Delta[q], "p_loo", "p_waic2")
#                 )


ssizes <- unique(dfr$ssize)
gofs <- unique(dfr$gof)
nssizes <- length(ssizes)
ngofs = length(gofs)
for (i in 1:nssizes){
  for (j in 1:ngofs){

   myssize = ssizes[i]
   mygof = gofs[j]
   df50 = subset(dfr, dfr$gof == mygof & dfr$ssize == myssize)
                  # dfr$model %in% c("GEV", "POT", "HBEV"))
      # df50 = subset(dfr, dfr$gof == mygof & dfr$ssize %in% c(20, 50))
      # myssize = 2050
                  # dfr$model != 'HBEVS'& dfr$model != 'HBEVD')

    pij = ggplot(df50, aes(x=model, y=value, fill=model)) +
         geom_boxplot(  alpha = 0.6) +
         scale_fill_manual(values=c("red", "green", "blue")) +
         # geom_jitter(width=0.25, alpha=0.5) +
      
       facet_grid(test ~ spec, scales='free', labeller = label_parsed) +
                    # labeller = label_parsed)

         theme_bw()  +
    # scale_color_viridis(name = expression(paste(q[50], '[mm]')))   +
           theme(
                 # legend.title = element_blank(),
                 legend.position = 'none',
                 text = element_text(size=15),
                 strip.background =element_rect(fill="white")
                 ) 
    
                # pij <- pij +  facet_wrap(test ~ spec, scales='free', labeller = label_parsed)
            # pij <- pij +  facet_grid(test ~ spec, scales='free_y',
            #         labeller = label_parsed)
            # 
    
      if (mygof == 'mwidth'){ # log scale only for this
       pij <- pij + ylab(expression(Delta[q[50]]))
      } else if (mygof == 'mbias') {
       pij <- pij + ylab(expression(b[q])) 
      } else if (mygof == 'fse') {
      pij <- pij + ylab('FSE')  
      } else {
      pij <- pij + ylab(mygof)  
      }
    
      if (mygof == 'mwidth'){ # log scale only for this
      # if (mygof == 'Delta[q]'){ # log scale only for this
      pij <- pij + scale_y_continuous(trans='log10')
      }
    
         pij <- pij + ggsave(file.path(outplot, 
                                       sprintf('synth_%s_ssize_%s.png',
                                       mygof, myssize)),
                                       width = 8, height = 5)
  }
}


# compute LPML - LPPD for IN SAMPLE data

dfd <- reshape2::dcast(dfr0, test + numj + ssize + spec + model + test_numj ~ gof, value.var = 'value')
dfd$enp <- (dfd$lpml - dfd$lppd)*dfd$ssize
dfd <- subset(dfd, dfd$test == 'ss' &
              dfd$model %in% c("gev", "pot_ppp", "wei_dgu_bin") &
              dfd$ss %in% c(50))
dfd$spec <- factor(dfd$spec, levels = c("gam", "wei", "wei_dgu", "gpd"),
                  labels = c("GAM", "WEI", 'WEI[G]',  "GP"))
dfd$model <- factor(dfd$model, levels = c("gev", "pot_ppp", "wei_dgu_bin"),
                  labels = c("GEV", "POT", "HMEV"))

pdfd <- ggplot(dfd, aes(x = model, y = enp, fill = model)) + 
  geom_boxplot(alpha = 0.6)+
  facet_wrap( ~spec, nrow=1, labeller = label_parsed) +
           geom_boxplot(  alpha = 0.6) +
         scale_fill_manual(values=c("red", "green", "blue")) +
         # geom_jitter(width=0.25, alpha=0.5) +
           theme_bw()  +
         ylab('Effective number of parameters')+
           #   theme(legend.position = 'bottom')
           theme(
                 # legend.title = element_blank(),
                 legend.position = 'none',
                 text = element_text(size=15),
                 strip.background =element_rect(fill="white")
                 )+ 

      scale_y_continuous(trans='log10')+
         ggsave(file.path(outplot, 
                                       sprintf('synth_eff_num_par.png')),
                                       width = 8, height = 4)
pdfd
# 
  