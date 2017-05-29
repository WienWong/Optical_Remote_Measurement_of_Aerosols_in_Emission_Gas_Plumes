timeElementsDOASButane <- function(month,day,START_MEASUREMENT,END_MEASUREMENT,Timevec) {
  # This function aims to calculate total time duration from beginning of flamespectrum to ending of the spectrum,
  # the time difference between two successive measurements and time duration within each measurement for Butane.
  # The csv format should be 'Butane_insb8_', and this function is especially for May 6 and May 17 where only 'Butane_insb8_'
  # format csv files exist.
  # 2017-02-21, 1st built, Weihua Wang
  
  dirpath='/home/wien/Octave/flameDOAS/GPS_SOF/'  
  
  # Calculate how many seconds during the flamespectrum measurement.
  TotalTime <- as.numeric(toString(difftime(strptime(END_MEASUREMENT, format="%H:%M:%OS"), 
                                            strptime(START_MEASUREMENT, format="%H:%M:%OS"), units = "secs") ) )
  
  daytime_Butane_st_ed_format <- sapply(1:length(Timevec), 
                                     function(kk) { if (3<=day & day<=9) {
                                       filpath = paste( dirpath, "160", toString(month), "0", toString(day), 
                                                        "/Butane_insb8_160", toString(month), "0", toString(day), Timevec[kk], "_0.csv",sep="")
                                     } else if (10<=day & day<=31) {
                                       filpath = paste( dirpath, "160", toString(month), toString(day), 
                                                        "/Butane_insb8_160", toString(month), toString(day), Timevec[kk], "_0.csv",sep="")
                                     }
                                       
                                       fil_Butane <- read.csv(filpath)
                                       names(fil_Butane) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                                                           "SolarZen","Windv","Windd","RMS","IgramNoise","concC2H4","concNH3","concC3H6","concTotalext","avgc/molec","Totalextconc","Light")
                                       len <- dim(fil_Butane)[1]
                                       daytime_Butane_begin <- as.numeric(as.character(fil_Butane[2,2]))        
                                       daytime_Butane_finish <- as.numeric(as.character(fil_Butane[len,2]))
                                       
                                       
                                       # Convert numeric format of xxxxx to string format of "x:xx:xx". e.g. 85638 to "8:56:38". 
                                       vec_daytime_Butane=c()
                                       if ( substr(daytime_Butane_begin,1,2) > 24 ){
                                         T_st <- paste( substr(daytime_Butane_begin,1,1), ":", substr(daytime_Butane_begin,2,3), ":", substr(daytime_Butane_begin,4,5), sep="" )
                                       } else {
                                         T_st <- paste( substr(daytime_Butane_begin,1,2), ":", substr(daytime_Butane_begin,3,4), ":", substr(daytime_Butane_begin,5,6), sep="" )
                                       }
                                       if ( substr(daytime_Butane_finish,1,2) > 24 ){
                                         T_ed <- paste( substr(daytime_Butane_finish,1,1), ":", substr(daytime_Butane_finish,2,3), ":", substr(daytime_Butane_finish,4,5), sep="" )
                                       } else {
                                         T_ed <- paste( substr(daytime_Butane_finish,1,2), ":", substr(daytime_Butane_finish,3,4), ":", substr(daytime_Butane_finish,5,6), sep="" )
                                       }
                                       vec_daytime_Butane = c(vec_daytime_Butane, c(T_st,T_ed))
                                       
                                       # Get a date-time object to calculate the time difference.
                                       vec_daytime_Butane_format <- strptime(vec_daytime_Butane, format="%H:%M:%OS")
                                       
                                       return(vec_daytime_Butane_format)
                                     }
  )
  
  # Calculate how many seconds from beginning of flamespectrum to beginning of 1st DOAS measurement. List uses [[]]
  BeginTimeDiff <- as.numeric( toString( difftime(daytime_Butane_st_ed_format[[1]][1], strptime(START_MEASUREMENT, format="%H:%M:%OS"), units = "secs") ) )
  
  Timedifvec = c(BeginTimeDiff) # put the beginning of time dif into a vector, then append this vector with timedif and slotdif.
  
  for (kk in 1:length(daytime_Butane_st_ed_format)) {
    
    TimeDiff <- as.numeric( toString( difftime( daytime_Butane_st_ed_format[[kk]][2], daytime_Butane_st_ed_format[[kk]][1], units = "secs" ) ) )
    Timedifvec = c(Timedifvec, TimeDiff)    # append time duration within each DOAS measurement
    
    if(kk+1 <= length(daytime_Butane_st_ed_format)) {
      SlotDiff <- as.numeric( toString( difftime(daytime_Butane_st_ed_format[[kk+1]][1], daytime_Butane_st_ed_format[[kk]][2], units = "secs") ) )
      Timedifvec = c(Timedifvec, SlotDiff)  # append time slot dif between two successive DOAS measurements
    }
    
    if(kk+1 > length(daytime_Butane_st_ed_format)){
      # Calculate how many seconds from ending of the last DOAS measurement to ending of flamespectrum.
      # When kk=5, daytime_NH3_st_ed_format[[kk]][2] is the last moment of last DOAS measurement. 
      # convert from "difftime" to "character", then ""character" to "numeric"
      EndTimeDiff <- as.numeric( toString( difftime(strptime(END_MEASUREMENT, format="%H:%M:%OS"), daytime_Butane_st_ed_format[[kk]][2], units = "secs") )  )
      
      Timedifvec = c(Timedifvec, EndTimeDiff)  # append the EndTimeDiff
      # The if block code breaks after calculating the EndTimeDiff
      break
    }
  }
  return( c(TotalTime, Timedifvec) )
}

# # Testing code
# month = 5; day = 17; START_MEASUREMENT = "09:35:37"; END_MEASUREMENT = "15:37:30";
# Timevec = c('1042', '1055', '1103', '1131', '1156', '1204', '1239', '1304', '1330',
#              '1347', '1425', '1435', '1447', '1505', '1530')  # remove last csv file at '1617'
# TV <- timeElementsDOASButane(month,day,START_MEASUREMENT,END_MEASUREMENT,Timevec)
# TV
# #
# month = 5; day = 6; START_MEASUREMENT = "08:50:33"; END_MEASUREMENT = "16:46:24"
# Timevec = c('0856', '1009', '1116', '1200', '1224', '1337', '1425', '1440', '1457', '1529')  # remove '0849' csv file at first
# TV <- timeElementsDOASButane(month,day,START_MEASUREMENT,END_MEASUREMENT,Timevec)
# TV
