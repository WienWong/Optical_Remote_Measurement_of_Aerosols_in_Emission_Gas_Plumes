timeElementsDOASO2 <- function(month,day,START_MEASUREMENT,END_MEASUREMENT,Timevec) {
  # This function aims to calculate total time duration from beginning of flamespectrum to ending of the spectrum,
  # the time difference between two successive measurements and time duration within each measurement for SO2, NO2 and O3 
  # 2017-01-11 first built, Weihua Wang
  
  #library(stringr)
  source("./strVectotimeStrvec.R")   # import a R function named 'strVectotimeStrvec'
  source("./numTtoStrT.R")           # import a R function named 'numTtoStrT'
  
  dirpath='/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/'
  
  # Calculate how many seconds during the flamespectrum measurement.
  TotalTime <- difftime(strptime(END_MEASUREMENT, format="%H:%M:%OS"), strptime(START_MEASUREMENT, format="%H:%M:%OS"), units = "secs")
  
  Timeformatvec <- strVectotimeStrvec(Timevec)
  
  # Calculate how many seconds from beginning of flamespectrum to beginning of 1st DOAS measurement.
  TimeFormatVec <- strptime(Timeformatvec, format="%H:%M:%OS")
  
  BeginTimeDiff <- as.numeric( toString( difftime(TimeFormatVec[1], strptime(START_MEASUREMENT, format="%H:%M:%OS"), units = "secs") ) )
  
  Timedifvec = c(BeginTimeDiff) # put the beginning of time dif into a vector, then append this vector with timedif and slotdif.
  
  for (kk in 1:length(Timevec)) {
    
    # Choose directory according to specific month and day
    if(1<=day & day<=9){
      filepath = paste( dirpath, "DOASeval_160", toString(month), "0", toString(day), "_",  Timevec[kk], ".csv",sep="")
    }else if(10<=day & day<=31){
      filepath = paste( dirpath, "DOASeval_160", toString(month), toString(day), "_",  Timevec[kk], ".csv",sep="")
    }
    
    dat = read.csv(filepath)
    # Make each variable (aka each column) readable
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","concHCHOa","concHCHOf","concHCHOb","concNO2a","concNO2f","concNO2b",
                    "concSO2c","concO3d", "concHCHOe", "concNO2e", "concTotalext","avgc/molec","Totalextconc","Light")
    
    # Extract interesting concentration
    daytime <- dat[,2]        # day time column
    timefromstart <- dat[,3]  # time from start of measurement column
    SO2conc <- dat[,23]       # SO2 concentration column
    O3conc <- dat[,24]        # O3 concentration column
    
    Tvec <- strptime(numTtoStrT(daytime), format="%H:%M:%OS") # Format time vector to calculate time difference.
    # Get a date-time object to calculate the time duration within each DOAS measurement.
    TimeDiff <- as.numeric( toString( difftime(Tvec[length(Tvec)], Tvec[1], units = "secs") ) )
    
    Timedifvec = c(Timedifvec, TimeDiff)    # append time duration within each DOAS measurement
    
    if(kk+1 <= length(Timeformatvec)) {
      SlotDiff <- as.numeric( toString( difftime(strptime(Timeformatvec[kk+1], format="%H:%M:%OS"), Tvec[length(Tvec)], units = "secs") ) )
      Timedifvec = c(Timedifvec, SlotDiff)  # append time slot dif between two successive DOAS measurements
    }
    
    if(kk+1 > length(Timeformatvec)){
      # Calculate how many seconds from ending of the last DOAS measurement to ending of flamespectrum.
      # When kk=15, Tvec[length(Tvec)] is the last moment of last DOAS measurement. The above if block code still runs, and this if block code will break after calculating the EndTimeDiff
      # convert from "difftime" to "character", then ""character" to "numeric"
      EndTimeDiff <- as.numeric( toString( difftime(strptime(END_MEASUREMENT, format="%H:%M:%OS"), Tvec[length(Tvec)], units = "secs") )  )
      
      Timedifvec = c(Timedifvec, EndTimeDiff)  # append the EndTimeDiff
      
      break
    }
  }
  return(c(TotalTime, Timedifvec)) 
}
