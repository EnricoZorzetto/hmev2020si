
# read all the stations in the USHCN - daily dataset and save them in csv 
# with daily precipitation, after fixing flags

rm(list=ls()) 
setwd( file.path('~','Projects','hbev','codes'))
source("hbev_module.R")    # main functions for data analysis
source("hbev_functions.R") # other functions
datadir = file.path('..', 'data', 'Data_GHCN_Daily')

downl = FALSE

if (downl){
  dir.create(datadir, showWarnings = FALSE)

  # list of files to download
  files2d = c('ghcnd-stations.txt',
              'ghcnd-version.txt',
              'ghcnd-states.txt',
              'ghcnd-stations.txt',
              'ghcnd-inventory.txt',
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

# read list of station positions and their length in years

# IV. FORMAT OF "ghcnd-stations.txt"

# ------------------------------
#   Variable   Columns   Type
# ------------------------------
#   ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSN FLAG     73-75   Character
# HCN/CRN FLAG 77-79   Character
# WMO ID       81-85   Character
# ------------------------------ 

ghcnd_stats <- file.path(datadir, 'ghcnd-stations.txt')


# dfs <- read.fwf(ghcnd_stats, widths = c(11, 8, 9, 6, 2, 10, 3, 3, 5), comment.char = "$", sep=, n=100)  
# dfs <- read.fwf(ghcnd_stats, widths = c(13, 10, 11, 8, 4, 12, 5, 5, 7), comment.char = "$", sep=, n=100)  
dfs <- read.fwf(ghcnd_stats, widths = c(12, 9, 10, 7, 3, 31, 4, 4, 6), comment.char = "")  
# dfs <- read.fwf(ghcnd_stats, widths = c(11, 8, 9, 6, 2, 10, 3, 3, 5), n = 1000)  
dfs_colnames <- c("ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", 
                  "NAME", "GSN FLAG", "HCN_CRN_FLAG", "WMO ID")
colnames(dfs) <- dfs_colnames

dfs_hcn <- subset(dfs, dfs$HCN_CRN_FLAG == 'HCN ') # beware of spaces!
# dfs_hcn$ID <- str_trim(as.character(dfs_hcn$ID))
dfs_hcn$ID <- gsub( " ", "", dfs_hcn$ID, fixed = TRUE)
# gsub(" ", "", x, fixed = TRUE)
dfs_hcn['START_YEAR'] = 0
dfs_hcn['END_YEAR'] = 0
dfs_hcn['NYEARS_ALL'] = 0
dfs_hcn['NYEARS_36'] = 0
dfs_hcn['MAXOBS'] = 0


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
  dlydir = file.path(datadir, 'ghcnd_hcn')
  files = list.files(dlydir)
  nfiles = length(files)
  print(sprintf("There are %s files", nfiles))
  
  # create output directory
  csvdir = file.path(datadir, 'extracted_csv')
  dir.create(csvdir, showWarnings = FALSE)
  
  
  # read one of them
  # myfile = files[1]
  for (k in 1:nfiles){
    print(k)
    myfile = files[k]
    
    
  dat0 = read.fwf(file.path(dlydir, myfile), widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)))  
   
  
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
  dat = subset(dat1, dat0$ELEMENT == 'PRCP') # get onlu precipitation
  dat$ELEMENT <- NULL
  colnames(dat)[colnames(dat)=="VALUE"] <- "PRCP"
  
  # qflags = as.vector(unique(dat$QFLAG))
  # mflags = unique(dat$MFLAG)
  # sflags = unique(dat$SFLAG)
  # print(qflags)
  # print(mflags)
  # print(sflags)

  
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
  
  # Q2remove = c("D", "G", "I", "K", "L", "M", "N", "O", "R", "S", "T", "W", "X", "Z")
  # remove values with specific flags - keep missing data (-9999)
  # dat3 = subset(dat, dat$QFLAG %in% Q2remove)
  # dat4 = subset(dat, !is.na(dat$QFLAG))
  
   # remove qflagged and missing PRCP
  dat4 = subset(dat, dat$QFLAG == " " & dat$PRCP > -1)
  nremoved = dim(dat)[1] - dim(dat4)[1]
  # print(sprintf("%s quality flagged or missing valued were removed", nremoved))
  dat4[['DATE']] = dat4$YEAR*10000 + dat4$MONTH*100 + dat4$DAY
  
  # create data and reorder for ascending date
  library(plyr)
  dat4 <- arrange(dat4,DATE)
  
  # keep precip in tenths of mm
  
  # save result in csv
  # outname = sub('nr$', 'nummer', 'artikelnr')
  outname = sub('dly$', 'csv', myfile)
  write.table(dat4, file = file.path(csvdir, outname), sep = ',', col.names=NA)
  
  start_year = min(dat4$YEAR)
  end_year = max(dat4$YEAR)
  nyears_all = length(unique(dat4$YEAR))
  maxobs = max(dat4$PRCP)
  
  # now compute number of years with less than 30 missing data,
  # inclusing as missing QUALITY- FLAGGED (removed above), -9999 or NA values
  maxmiss = 30
  min_nevents = 0
  Nt = 366
  df36 = load_obs_data(dat4, readdata=FALSE,
                          maxmiss = maxmiss, min_nevents = min_nevents,
                          dividebyten = TRUE , Nt = Nt)
  nyears_36 = length(unique(df36$YEAR)) 
  
  statname = sub('.dly$', '', myfile)
  dfs_hcn$START_YEAR[dfs_hcn$ID == statname] = start_year
  dfs_hcn$END_YEAR[dfs_hcn$ID == statname] = end_year
  dfs_hcn$NYEARS_ALL[dfs_hcn$ID == statname] = nyears_all
  dfs_hcn$NYEARS_36[dfs_hcn$ID == statname] = nyears_36
  dfs_hcn$MAXOBS[dfs_hcn$ID == statname] = maxobs

  }
  
# save   
  outnamest = 'list_ushcn_stats.csv'
  write.table(dfs_hcn, file = file.path(datadir, outnamest), sep = ',', col.names=NA)
  
  
   