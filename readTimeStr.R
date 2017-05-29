readTimeStr <- function(mn,dy,len,nmrows,tpdir,ir_st) {
  
  # This function will import all the time information in all the recorded STD files into a character matrix.
  # 2016-12-24, 1st bulit, 2017-01-18, revisit, by Weihua Wang
  # mn: month
  # dy: day
  # len: number of used STD files (for plotting)
  # tpdir: the temp directory in charactor contains all the STD files
  # ir_st: 1st STD file used (for IR plotting)
  # nmrows: total #. of STD files for the specific day
  # select "spectra" in dirpath function
  
  source("./dirpath.R")
  fSTD = matrix(NA, nrow=len, ncol=2)   # pre-locate a NA matrix
  # If 1st STD file used for IR plotting between 0.STD and 9.STD
  if(0<=ir_st & ir_st <=9 ) {
    if ( (nmrows-1)>=100 & (nmrows-1)<=999 ) {
      for ( aa in c(ir_st:9) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000000', as.character(aa), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[aa-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[aa-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( bb in c(10:99) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00000', as.character(bb), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[bb-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[bb-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( mm in c(100:(nmrows-1)) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
    } else if ( (nmrows-1)>=1000 & (nmrows-1)<=9999 ) {
      for ( aa in c(ir_st:9) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000000', as.character(aa), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[aa-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[aa-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( bb in c(10:99) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00000', as.character(bb), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[bb-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[bb-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( mm in c(100:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:(nmrows-1)) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
    } else if ( (nmrows-1)>=10000 & (nmrows-1)<=99999 ) {
      for ( aa in c(ir_st:9) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000000', as.character(aa), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[aa-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[aa-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( bb in c(10:99) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00000', as.character(bb), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[bb-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[bb-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( mm in c(100:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:9999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( vv in c(10000:(nmrows-1)) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00', as.character(vv), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[vv-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[vv-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
    }
    
  } else if ( 10<=ir_st & ir_st <=99 ){
    if ( (nmrows-1)>=1000 & (nmrows-1)<=9999 ) {
      
      for ( bb in c(ir_st:99) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00000', as.character(bb), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[bb-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[bb-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( mm in c(100:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:(nmrows-1) ) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
    } else if ( (nmrows-1)>=10000 & (nmrows-1)<=99999 ) {
      
      for ( bb in c(ir_st:99) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00000', as.character(bb), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[bb-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[bb-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( mm in c(100:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:9999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( vv in c(10000:(nmrows-1)) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00', as.character(vv), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[vv-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[vv-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
    }
  } else if ( 100<=ir_st & ir_st <=999 ) {
    if ( (nmrows-1)>=1000 & (nmrows-1)<=9999 ) {
      
      for ( mm in c(ir_st:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:nmrows-1) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
    } else if ( (nmrows-1)>=10000 & (nmrows-1)<=99999 ) {
      
      for ( mm in c(ir_st:999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S0000', as.character(mm), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[mm-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[mm-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( nn in c(1000:9999) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
      for ( vv in c(10000:(nmrows-1)) ){
        filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00', as.character(vv), '.STD', sep="")
        S = read.table(filepath)
        ST = toString( S[c(2058,2059), ] )
        fSTD[vv-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
        fSTD[vv-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
      }
      
    }
  } else if ( 1000<=ir_st & ir_st <=9999) {
    for ( nn in c(ir_st:9999) ){
      filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S000', as.character(nn), '.STD', sep="")
      S = read.table(filepath)
      ST = toString( S[c(2058,2059), ] )
      fSTD[nn-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
      fSTD[nn-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
    }
    
    for ( vv in c(10000:(nmrows-1)) ){
      filepath = paste(dirpath(mn,dy,"spectra"), tpdir, '/S00', as.character(vv), '.STD', sep="")
      S = read.table(filepath)
      ST = toString( S[c(2058,2059), ] )
      fSTD[vv-ir_st+1,1] = substr(unlist(str_split(ST,", "))[1],1,8)
      fSTD[vv-ir_st+1,2] = substr(unlist(str_split(ST,", "))[2],1,8)
    }
  }
  return(fSTD)
}
