calTOTime <- function(Start_Measurement, End_Measurement) {
  
  # Calculate how many seconds in total uring each 'Flame' spectra measurement.
  # 2017-03-07, 1st built, Weihua Wang
  
  ToTime <- as.numeric( toString(difftime(strptime(End_Measurement, format="%H:%M:%OS"), 
                                          strptime(Start_Measurement, format="%H:%M:%OS"), units = "secs") ))
  return(ToTime)
}
