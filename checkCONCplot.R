
# This function aims to check SOF concentration plot of Alkane or Alkene or NH3.
# The plot needs to be compared with the RMS checking plot by applying 'spikesfromRMS2.R'
# 2017-06-12, 1st built, Wang Weihua
# filename has format of, e.g. "Butane05_d05_1605081338_0"
# width is the length of the SpikeFilter
# k is the day and mon is the month.

checkCONCplot <- function(filename,width=10,k=8,mon=5) {
  
  source("./dirpath.R")
  source("./spikesfromRMS2.R")
  source("./spikesReplaced.R")
  
  dirpath <- dirpath(mon,k,"gps")
  options(digits=9)                      # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  
  dat <- dat[-1, ]                      # remove the 1st row containning NAs.
  
  FLAG="OFF"                            # set the flag equals "OFF" thus do not need to checking plot the RMS.
  
  idxVec <- spikesfromRMS2(filename,FLAG,width,k)
  
  gas = ""
  if(substr(filename, 1, 3) == "But"){
    gas = "Alkane"
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","Propane","Butane","Octane","Totalextconc","avgc/molec","Totalextflux","Light")

    Time <- dat[,3]
    conCen <- dat[,21]          

    plot(Time,conCen,type='l',xlab='TimeFromStart',ylab='Concentration',main='Total Alkane Concentration',col='blue')
    conCen <- spikesReplaced(conCen, idxVec)
    lines(Time,conCen,type='l',col='red')
    
  } else if(substr(filename, 1, 3) == "but"){
    gas = "Alkene"
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","butadiene","Totalextconc","avgc/molec","Totalextflux","Light")
   
    Time <- dat[,3]
    conCen <- dat[,18] + dat[,20] + dat[,21] # extract sum concentration of C2H4,C3H6,C4H6       
    
    plot(Time,conCen,type='l',xlab='TimeFromStart',ylab='Concentration',main='Total Alkene Concentration',col='blue')
    conCen <- spikesReplaced(conCen, idxVec)
    lines(Time,conCen,type='l',col='red')
     
  } else if(substr(filename, 1, 3) == "C3H"){
    gas = "NH3"
    names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","Totalextconc","avgc/molec","Totalextflux","Light")
    
    Time <- dat[,3]
    conCen <- dat[,19]                      # extract concentration of NH3
    
    plot(Time,conCen,type='l',xlab='TimeFromStart',ylab='Concentration',main='Total Ammonia Concentration',col='blue')
    conCen <- spikesReplaced(conCen, idxVec)
    lines(Time,conCen,type='l',col='red')
  }
  
}

# Testing code
# checkCONCplot("Butane05_d05_1605081338_0")
# checkCONCplot("Butane05_d05_1605081420_0")
# checkCONCplot("Butane05_d05_1605081518_0")
# checkCONCplot("C3H6_d05_1605081338_0")
# checkCONCplot("C3H6_d05_1605081420_0")
# checkCONCplot("C3H6_d05_1605081518_0")
# checkCONCplot("butadiene_d05_1605081338_0")
# checkCONCplot("butadiene_d05_1605081420_0")
# checkCONCplot("butadiene_d05_1605081518_0")
