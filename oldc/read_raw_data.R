# read all the stations in the USHCN - daily dataset and save them in csv 
# with daily precipitation, after fixing flags

rm(list=ls()) 
# setwd( file.path('~','Projects','hbev','codes'))
setwd( file.path('~','Projects','jasa_SI_hmev','codes'))
# source("hbev_module.R")    # main functions for data analysis
# source("hbev_functions.R") # other functions

downl = FALSE

if (downl){
  datadir1 = file.path('..', 'data')
  dir.create(datadir1, showWarnings = FALSE)
  datadir = file.path('..', 'data', 'Data_GHCN_Daily')
  dir.create(datadir, showWarnings = FALSE)

  # list of files to download
  files2d = c('ghcnd-stations.txt',
              'ghcnd-version.txt',
              'ghcnd-states.txt',
              'ghcnd-stations.txt',
                 'readme.txt',
              'ghcnd_hcn.tar.gz'
              )
  # download files from NOAA directory
  url <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily"
  for (file in files2d) {
    download.file( file.path(url, file), file.path(datadir, file))
  }
  # unzip station files
  system("tar xzvf ../data/Data_GHCN_Daily/ghcnd_hcn.tar.gz -C ../data/Data_GHCN_Daily/")
}

# Format of the data:

# ------------------------------
#   Variable   Columns   Type
# ------------------------------
#   ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
# .           .          .
# .           .          .
# .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------

# read stations and save csv with daily precipitation data
  # dlydir = file.path('..', 'data', 'Data_GHCN_Daily', 'ghcnd_hcn')
# read old dataset instead
  dlydir = file.path('~','Projects','hbev', 'data', 'Data_GHCN_Daily', 'ghcnd_hcn')
  files = list.files(dlydir, pattern = "\\.dly$")
  nfiles = length(files)
  print(sprintf("There are %s files", nfiles))
  
  # create output directory
  csvdir = file.path('..', 'data', 'Data_GHCN_Daily', 'extracted_csv')
  dir.create(csvdir, showWarnings = FALSE)
  

  # # read one of them
  for (iff in 1:nfiles) {
  myfile = files[iff]
  dat0 = read.fwf(file.path(dlydir, myfile),
                  widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)))

  
    cnames <- c("STATION", "YEAR", "MONTH", "ELEMENT") 
    cnames0 = cnames
    vals = c("VALUE", "MFLAG", "QFLAG", "SFLAG")
    nvals = length(vals)
  for (i in 1:31){
    for (j in 1:nvals){
     cnames = c(cnames, paste(vals[j], toString(i), sep='.') )
    }
  }
    varying = setdiff(cnames, cnames0)
    colnames(dat0) <- cnames
  
  dat1 = reshape(dat0, idvar = c('STATION', "ELEMENT", "YEAR", "MONTH"), 
                      # varying = list(c(2,4), c(3,5)), 
                      # varying = list(posA, posB, posC, posD), 
                      times = unlist(lapply(1:31, toString)),
                      timevar = 'DAY',
                      varying = varying,
                      # v.names = c("VALUE", "MFLAG", "QFLAG", "SFLAG"), 
                      direction = "long")  
  
  # keep only precipitation
  dat = subset(dat1, dat1$ELEMENT == 'PRCP') # get onlu precipitation
  dat$ELEMENT <- NULL
  colnames(dat)[colnames(dat)=="VALUE"] <- "PRCP"
  
  qflags = as.vector(unique(dat$QFLAG))
  mflags = unique(dat$MFLAG)
  sflags = unique(dat$SFLAG)
  print(qflags)
  print(mflags)
  print(sflags)
  

  
  # Blank = did not fail any quality assurance check
  # D     = failed duplicate check
  # G     = failed gap check
  # I     = failed internal consistency check
  # K     = failed streak/frequent-value check
  # L     = failed check on length of multiday period 
  # M     = failed megaconsistency check
  # N     = failed naught check
  # O     = failed climatological outlier check
  # R     = failed lagged range check
  # S     = failed spatial consistency check
  # T     = failed temporal consistency check
  # W     = temperature too warm for snow
  # X     = failed bounds check
  # Z     = flagged as a result of an official Datzilla 
  # investigation
  
  # Q2remove = c("D", "G", "I", "K", "L", "M", "N",
  # "O", "R", "S", "T", "W", "X", "Z")
  # remove values with specific flags - keep missing data (-9999)
  # dat4 = subset(dat, !(dat$QFLAG %in% Q2remove))
  # dat4 = subset(dat, !is.na(dat$QFLAG))
  dat4 = subset(dat, dat$QFLAG == " ")
  # dat4 = subset(dat, dat$MFLAG == " ")
  nremoved = dim(dat)[1] - dim(dat4)[1]
  print(sprintf("%s quality flagged valued were removed", nremoved))
  dat4[['DATE']] = dat4$YEAR*10000 + dat4$MONTH*100 + dat4$DAY
  dat4 = subset(dat4, dat4$PRCP > -1)
  
  
  # create data and reorder for ascending date
  # library(plyr)
  dat4 <- arrange(dat4,DATE)
  
  outname = sub('dly$', 'csv', myfile)
  write.table(dat4, file = file.path(csvdir, outname), sep = ',')
  
  
  }
  
  
   

