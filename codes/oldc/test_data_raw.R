

# read data from old folder:
    olddir = file.path('~', 'Projects', 'hbev','data', 'Data_GHCN_Daily', 'extracted_csv')
    oldfiles = list.files(olddir)
    noldfiles = length(oldfiles)
    print(sprintf('num old files = %s', noldfiles))
    
    newdir = file.path('..', 'data', 'Data_GHCN_Daily', 'extracted_csv')
    newfiles = list.files(newdir)
    nnewfiles = length(newfiles)
    print(sprintf('num old files = %s', nnewfiles))
    
    for (i in 1:nnewfiles){
    # for (i in 1:20){
      myfile = oldfiles[i]
      # newfile = newfiles[i]
      # if (myfile != oldfile){
        # print('different!')
      # }
      
      dfo = read.csv( file.path(olddir, myfile))
        dfn = read.csv( file.path(newdir, myfile))
      
      dimo = dim(dfo)[1]
      dimn = dim(dfn)[1]
      sumo = sum(dfo$PRCP)
      sumn = sum(dfn$PRCP)
      if (dimo != dimn){
        print(sprintf('different sizes = %s %s!', dimo, dimn))
      }
      if (dimo != dimn){
        print(sprintf('different sums = %s %s!', sumo, sumn))
      }
      # read both if exixts and check dimension
    }
    
    unique(dfn$MFLAG)
    unique(dfo$MFLAG)
    unique(dfn$QFLAG)
    unique(dfo$QFLAG)
    
min(dfo$PRCP)
min(dfn$PRCP)

df9 = dfn[dfn$PRCP < 0]

dfdates = setdiff(dfo$PRCP, dfn$DATE)
