

# This function aims to locate the spikes from the RMS column and return the spikes' indices if exist.
# E.g. "Butane05_d05_1605081338_0.csv" have abnormal RMS values. Some RMS values equal 10, however, 10s seem 
# do not record any concentration, thus do not affect my previous plotting.
# 2017-06-08, 1st built, Wang Weihua

spikesfromRMS2 <- function(filename,k=8,mon=5) {
  
  source("./dirpath.R")
  source("./SpikeFilter.R")
  
  # # for debugging purpose 
  # filename = "Butane05_d05_1605081518_0"; k=8; mon=5; ch="T"  
  # filename = "Butane05_d05_1605081420_0"; k=8; mon=5; ch="T"
  # filename = "Butane05_d05_1605081338_0"; k=8; mon=5; ch="T"
  # filename = "butadiene_d05_1605081518_0"; k=8; mon=5; ch="T"
  
  dirpath <- dirpath(mon,k,"gps")
  options(digits=9)                      # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  
  if(substr(filename, 1, 3) == "But"){
    
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","Propane","Butane","Octane","Totalextconc","avgc/molec","Totalextflux","Light")
    
  } else if(substr(filename, 1, 3) == "but"){
    
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","butadiene","Totalextconc","avgc/molec","Totalextflux","Light")
    
  } else if(substr(filename, 1, 3) == "C3H"){
    
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","Totalextconc","avgc/molec","Totalextflux","Light")
    
  }
    
  dat <- dat[-1, ]
  width=4
  rr <- SpikeFilter(dat$RMS,width)  # HampelFilter will filter out some small peaks that are not outliers.
  plot(dat$TimeFromStart, dat$RMS, type='l', col='blue')
  lines(dat$TimeFromStart, rr[[1]], col='red', type='l')
  ind <- rr[[2]]
  ind
  
  return(ind)
  
}

# Testing code
#spikesfromRMS2("Butane05_d05_1605081420_0",k=8,mon=5,ch="T")

