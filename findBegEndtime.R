
findBegEndtime <- function(day, month, tempdir, numRows) {
  
  # This function finds the starting and ending time of the 'Flame' spectra measurement
  # 2017-03-07, 1st built, Weihua Wang
  # day: specific day
  # month: specific month
  # tempdir: file name where the 'Flame' spectra are stored
  # numRows: number of total STD files in each file
  #
  # S0 means the 1st STD file while Sx means the last one.
  
  
  library(stringr)         # for using function of "str_split".
  source("./dirpath.R")
  
  if(length(tempdir)==1 & length(numRows)==1) {
    
    S0 = read.table(paste( dirpath(month,day,"spectra"), tempdir, "/S0000000", ".STD",sep="") )  
    ST0 = toString( S0[c(2058,2059), ] )       # row of 2058 and 2059 contain the time information.
    ST_start = unlist(str_split(ST0,", "))[1]  # 
    Start_Measurement <- substr(ST_start,1,8)  # ST_start results in e.g. "08:55:55.400000" but I just need e.g. "08:55:55"
    
    if(numRows>=10000 & numRows<=99999) {
      Sx = read.table(paste( dirpath(month,day,"spectra"), tempdir, "/S00", toString(numRows-1), ".STD",sep="") )
      STx = toString( Sx[c(2058,2059), ] )
      ST_end = unlist(str_split(STx,", "))[2]                                 # 
      End_Measurement <- substr(ST_end,1,8)    # ST_end results in e.g. "17:12:35.237000" but I just need e.g. "17:12:35"
    }
    
    return( c(Start_Measurement, End_Measurement) )
    
  } else if (length(tempdir==2) & length(numRows)==2) { # This is because sometimes they divided the measurements into 2 parts.
    # First measurements 
    S0 = read.table(paste( dirpath(month,day,"spectra"), tempdir[1], "/S0000000", ".STD",sep="") )  
    ST0 = toString( S0[c(2058,2059), ] )      
    ST_start = unlist(str_split(ST0,", "))[1]                      
    Start_Measurement <- substr(ST_start,1,8) 
    
    if(numRows[1]>=100 & numRows[1]<=999) {
      Sxa = read.table(paste( dirpath(month,day,"spectra"), tempdir[1], "/S0000", toString(numRows[1]-1), ".STD",sep="") )
      STxa = toString( Sxa[c(2058,2059), ] )
      ST_enda = unlist(str_split(STxa,", "))[2]                              
      End_MeasurementA <- substr(ST_enda,1,8)
    } else if(numRows[1]>=1000 & numRows[1]<=9999) {
      Sxa = read.table(paste( dirpath(month,day,"spectra"), tempdir[1], "/S000", toString(numRows[1]-1), ".STD",sep="") )
      STxa = toString( Sxa[c(2058,2059), ] )
      ST_enda = unlist(str_split(STxa,", "))[2]                              
      End_MeasurementA <- substr(ST_enda,1,8)
    } else if(numRows[1]>=10000 & numRows[1]<=99999) {
      Sxa = read.table(paste( dirpath(month,day,"spectra"), tempdir[1], "/S00", toString(numRows[1]-1), ".STD",sep="") )
      STxa = toString( Sxa[c(2058,2059), ] )
      ST_enda = unlist(str_split(STxa,", "))[2]                             
      End_MeasurementA <- substr(ST_enda,1,8)  
    }
    # Second measurements 
    S0b = read.table(paste( dirpath(month,day,SW), tempdir[2], "/S0000000", ".STD",sep="") )
    ST0b = toString( S0b[c(2058,2059), ] )
    ST_startb = unlist(str_split(ST0b,", "))[1]
    Start_MeasurementB <- substr(ST_startb,1,8)  
    
    if(numRows[2]>=100 & numRows[2]<=999) {
      Sx = read.table(paste( dirpath(month,day,SW), tempdir[2], "/S0000", toString(numRows[2]-1), ".STD",sep="") )
      STx = toString( Sx[c(2058,2059), ] )
      ST_end = unlist(str_split(STx,", "))[2]
      End_Measurement <- substr(ST_end,1,8) 
    } else if(numRows[2]>=1000 & numRows[2]<=9999) {
      Sx = read.table(paste( dirpath(month,day,SW), tempdir[2], "/S000", toString(numRows[2]-1), ".STD",sep="") )
      STx = toString( Sx[c(2058,2059), ] )
      ST_end = unlist(str_split(STx,", "))[2]
      End_Measurement <- substr(ST_end,1,8) 
    } else if(numRows[2]>=10000 & numRows[2]<=99999) {
      Sx = read.table(paste( dirpath(month,day,SW), tempdir[2], "/S00", toString(numRows[2]-1), ".STD",sep="") )
      STx = toString( Sx[c(2058,2059), ] )
      ST_end = unlist(str_split(STx,", "))[2]
      End_Measurement <- substr(ST_end,1,8) 
    }
    
    return( c(Start_Measurement, End_MeasurementA, Start_MeasurementB, End_Measurement) )
  }

}
