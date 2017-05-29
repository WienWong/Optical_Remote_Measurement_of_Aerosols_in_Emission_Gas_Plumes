dirpath <- function(month,day,SW) {
  # Om Mani Padme Hum !
  # This function will generate a directory path either from the 'Flame' spectra measurement, the DOAS spectra measurement,
  # or the GPS/SOF measurement.
  # 2016-12-24, 1st built, 2017-02-02, 2nd modified, Wang Weihua
  # SW stands for switch, and select either the 'Flame' spectra folder ("spectra") or the DOAS folder ("doas") or the GPS_SOF
  # folder ("gps")
  
  if (SW=="spectra") {
    if (1<=day & day<=9){
      tmpath <- paste('/home/wien/Octave/flameDOAS/spectra/160', toString(month), '0', toString(day), '/', sep="")
    } else if (10<=day & day<=31){
      tmpath <- paste('/home/wien/Octave/flameDOAS/spectra/160', toString(month), toString(day), '/', sep="")
    }
    return(tmpath)
  } else if (SW=="doas") {
    if (1<=day & day<=9){
      tmpath <- paste('/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/DOASeval_160', toString(month), '0', toString(day), '_', sep="")
    } else if (10<=day & day<=31){
      tmpath <- paste('/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/DOASeval_160', toString(month), toString(day), '_', sep="")
    }
    return(tmpath) 
  } else if (SW=="gps") {
    if (1<=day & day<=9){
      tmpath <- paste( "/home/wien/Octave/flameDOAS/GPS_SOF/160", toString(month), '0', toString(day), "/", sep="")
    } else if (10<=day & day<=31){
      tmpath <- paste( "/home/wien/Octave/flameDOAS/GPS_SOF/160", toString(month), toString(day), "/", sep="")
    }
    return(tmpath) 
  }
  
}

## Test
# dirpath(5,13,"spectra")
# dirpath(5,5,"spectra")
# dirpath(5,13,"doas")
# dirpath(5,5,"doas")
# dirpath(5,13,"gps")
# dirpath(5,5,"gps")
