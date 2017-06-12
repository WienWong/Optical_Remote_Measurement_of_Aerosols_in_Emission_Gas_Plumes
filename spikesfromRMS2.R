
# This function aims to locate the spikes from the RMS column and return the spikes' indices if exist.
# It plots out the RMS vs time before-and-after applying spikes removed method if flag equals "ON".
# When flag equals to "OFF", it facilitates the concentration checking plot.
# E.g. "Butane05_d05_1605081338_0.csv" have abnormal RMS values. Some RMS values equal 10, however, 10s seem 
# do not record any concentration, thus do not affect my previous plotting.
# 2017-06-08, 1st built, 2017-06-12, 2nd modified, Wang Weihua

spikesfromRMS2 <- function(filename,flag="ON",width=10,k=8,mon=5) {
  
  source("./dirpath.R")
  source("./SpikeFilter.R")
  
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
  
  rr <- SpikeFilter(dat$RMS,width)  # HampelFilter will filter out some small peaks that are not outliers.
  if(flag=="ON"){
    plot(dat$TimeFromStart, dat$RMS, xlab='TimeFromStart', ylab='RMS', main='RMS checking plot', type='l', col='blue')
    lines(dat$TimeFromStart, rr[[1]], col='red', type='l')
    ind <- rr[[2]]
    ind
    return(ind)
  } else if(flag=="OFF"){
    ind <- rr[[2]]
    ind
    return(ind)
  }

}

# Testing code
# spikesfromRMS2("Butane05_d05_1605081420_0")
# spikesfromRMS2('Butane05_d05_1605081420_0',"OFF")
# spikesfromRMS2('Butane05_d05_1605081338_0',"ON")
# spikesfromRMS2('Butane05_d05_1605081338_0',"OFF")
# spikesfromRMS2('Butane05_d05_1605081518_0')
# spikesfromRMS2('Butane05_d05_1605081518_0',"OFF")
# spikesfromRMS2('butadiene_d05_1605081518_0')
# spikesfromRMS2('butadiene_d05_1605081518_0',"OFF")
