
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)


dset = 'kfold_479'
minlength = 100 # min length of stations included in the analysis
# setwd( file.path('~','Projects','hbev','codes'))

# outdata = file.path('..', 'output', 'output_data_0', sprintf('warnings_%s', dset))
# outdata = file.path('..', 'output', 'output_data_0dec', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_1', sprintf('output_%s', dset))
# outdata = file.path('..', 'output', 'output_data_backup_synth', sprintf('output_%s', dset))
outdata = file.path('..', 'output', 'output_data', sprintf('warnings_%s', dset))
files <- list.files(outdata)
nfiles <- length(files)
# read and count warnings for different models

datalist <-  list()
# read in sequence and concat data
for (i in 1:nfiles) {
  dat <-  read.table( file.path(outdata, files[i]), sep = ',', 
                    header = TRUE, row.names = 1)
  datalist[[i]] <- dat # add it to your list
}
dfw <- data.table::rbindlist(datalist)

# mymodel <- subset(dfw, dfw$model == 'wei_dgu_bin')
mymodel <- subset(dfw, dfw$model == 'gev')
count = mymodel$number[mymodel$number > 0]

