
# This function aims to locate the spikes from the RMS column and return the spikes' indices if exist(s).
# Note that the index should add one which corresponds to the row index in the DOAS csv file.
# For example, spikesfromRMS("154013",k=8,mon=5,ch="T") gives 830 928. The 831th and 929th row in 
# "DOASeval_160508_154013.csv" have abnormal RMS values.
# 2017-06-01, 1st built, Wang Weihua

spikesfromRMS <- function(filename,k=8,mon=5,ch="T") {
  
  source("./dirpath.R")
  source("./SpikeFilter.R")
  #source("./spikesReplaced.R")
  
  #filename = "154013"; zoom=11; k=8; mon=5; ch="T"  # for debugging purpose 

  dirpath <- dirpath(mon,k,"doas")
  options(digits=9)                      # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  # Name each variable                   # modified on 2017-02-02
  names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                  "SolarZen","Windv","Windd","RMS","HCHOaconc","HCHOfconc","HCHObconc","NO2aconc","NO2fconc","NO2bconc","SO2conc",
                  "O3conc","HCHOeconc","NO2econc","Totalextconc","avgc/molec","Totalextflux","Light")
                  
  # width=3  
  # rr <- HampelFilter(dat$RMS,width)
  # plot(dat$TimeFromStart, dat$RMS, type='l', col='blue')
  # lines(dat$TimeFromStart, rr[[1]], col='red', type='l')
  # rr[[2]]
  
  width=3
  rr <- SpikeFilter(dat$RMS,width)       # HampelFilter will filter out some small peaks that are not outliers.
  #plot(dat$TimeFromStart, dat$RMS, type='l', col='blue')
  #lines(dat$TimeFromStart, rr[[1]], col='red', type='l')
  ind <- rr[[2]]
  ind
  
  return(ind)
  
}


spikesfromRMS("133927",k=8,mon=5,ch="T")
spikesfromRMS("142100",k=8,mon=5,ch="T")
spikesfromRMS("154013",k=8,mon=5,ch="T")
