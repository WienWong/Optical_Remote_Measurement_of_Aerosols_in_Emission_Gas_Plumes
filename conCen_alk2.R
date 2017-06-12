
# This function is an updated version of 'conCen_alk.R'
# NH3 or Alkene (includes C2H4-Ethylene, C3H6-Propene, C4H6-Butadiene) or Alkane (C3H8-Propane,
# C4H10-Butane, C8H18-Octane) concentration plot along the driving route on May kth 2016
# 2016-06-12, 1st built, Weihua Wang. 
# k is a number specifing the day from 1 to 31. mon is the month (5 or 6)
# filename should be a string like 'butadiene_d05_1605081338_0' or "Butane05_d05_1605081420_0"
# or 'C3H6_d05_1605151309_0'
# The limit of zoomfactor is 1 to 21, default value is 14.
# 'width' controls the SpikeFilter length to removing the spikes.
# segflag: If "ON", endSeg is the last time instant. 
# endSeg: the ending time instant, e.g."141618". It is used to subset the data

conCen_alk2 <- function(k,filename,sizer="OFF",zoomfactor=14,width=10,segflag="OFF",endSeg="141618",mon=5){
  
  library(ggplot2)
  library(ggmap) 
  source("./dirpath.R")
  source("./spikesfromRMS2.R")
  source("./spikesReplaced.R")
  
  dirpath <- dirpath(mon,k,"gps")
  options(digits=9)                      # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  
  datNArm <- dat[-1, ]                   # remove the 1st row containning NAs.
  
  FLAG="OFF"                             # set the flag equals "OFF" thus do not need to checking plot the RMS.

  idxVec <- spikesfromRMS2(filename,FLAG,width,k)
  
  gas = "";
  if(substr(filename, 1, 3) == "But"){
    gas = "Alkane"
    names(datNArm) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","Propane","Butane","Octane","TotalConc","avgc/molec","Totalextflux","Light")

    Daytime <- datNArm[,2]
    posLat <- datNArm[,8]                # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]                # extract longitude 
    conCen <- datNArm$TotalConc               # extract Total Alkane concentration
    #class(datNArm[,2])                  # "factor"
    #class(datNArm[,21])                 # "numeric"
    daytime <- as.vector.factor(Daytime) # convert a factor to a character vector
    dd <- as.numeric(daytime)
    time_st = dd[1]
    time_ed = dd[length(daytime)]
    
    datNArm$TotalConc <- spikesReplaced(conCen, idxVec) # new concentration with spikes removed and replaced  
    
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
    
    if(segflag=="OFF"){
      coordinate[, 1] <- posLon            # longitude
      coordinate[, 2] <- posLat            # latitude
      coordinate[, 3] <- datNArm$TotalConc # concentration  
      coor <- data.frame(coordinate)       # need data frame not matrix format
      names(coor) <- c('longitude','latitude','concentration') # add names 
    } else if(segflag=="ON"){
      coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(TotalConc, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-03 
    } else if(segflag=="22"){
      coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(TotalConc, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-04
    } 
    
    coor[coor < 0.01 | coor > 1e4] <- 0.01  # further filtering for very positive or negative concentrations 
    
    coor$size <- coor$concentration    # for sizer plot
    
    saveRDS(coor, "datmrgAlkane.rds")  # save for later use
    
  } else if(substr(filename, 1, 3) == "but"){
    gas = "Alkene"
    names(datNArm) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","butadiene","TotalConc","avgc/molec","Totalextflux","Light")
    
    Daytime <- datNArm[,2]
    posLat <- datNArm[,8]     # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]     # extract longitude 
    conCen <- datNArm[,18] + datNArm[,20] + datNArm[,21] # extract a sum concentration of C2H4,C3H6,C4H6 
    #class(conCen)            # "numeric"
    datNArm$conCen <- conCen  # add one column variable for subset 
    colnames(datNArm)[26] <- "conCen" # name this newly added column
    
    daytime <- as.vector.factor(Daytime) 
    dd <- as.numeric(daytime)
    time_st = dd[1]
    time_ed = dd[length(daytime)]

    datNArm$conCen <- spikesReplaced(conCen, idxVec)
    
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
    
    if(segflag=="OFF"){
      coordinate[, 1] <- posLon          # longitude
      coordinate[, 2] <- posLat          # latitude
      coordinate[, 3] <- datNArm$conCen  # concentration  
      coor <- data.frame(coordinate)   
      names(coor) <- c('longitude','latitude','concentration') 
    } else if(segflag=="ON"){
      coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(conCen, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') 
    } else if(segflag=="22"){
      coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(conCen, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') 
    }                                       
    
    coor[coor < 0.01 | coor > 1e4] <- 0.01  
    
    coor$size <- coor$concentration
    
    saveRDS(coor, "datmrgAlkene.rds")
    
  } else if(substr(filename, 1, 3) == "C3H"){
    gas = "NH3"
    names(datNArm) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                    "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4","NH3","C3H6","TotalConc","avgc/molec","Totalextflux","Light")

    Daytime <- datNArm[,2]
    posLat <- datNArm[,8]                 # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]                 # extract longitude 
    conCen <- datNArm[,19]                # extract concentration of NH3 
    daytime <- as.vector.factor(Daytime)  # convert a factor to a character vector
    dd<-as.numeric(daytime)
    time_st = dd[1]
    time_ed = dd[length(daytime)]

    datNArm$conCen <- spikesReplaced(conCen, idxVec)
    
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
    
    if(segflag=="OFF"){
      coordinate[, 1] <- posLon           # longitude
      coordinate[, 2] <- posLat           # latitude
      coordinate[, 3] <- datNArm$conCen   # concentration  
      coor <- data.frame(coordinate)      # need data frame not matrix format
      names(coor) <- c('longitude','latitude','concentration') # add names 
    } else if(segflag=="ON"){
      coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(NH3, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-03 
    } else if(segflag=="22"){
      coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(NH3, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-04
    } 
    
    coor[coor < 0.01 | coor > 1e4] <- 0.01  # further filtering for very positive or negative concentrations, modified on 2017-03-02
    
    coor$size <- coor$concentration
    
    saveRDS(coor, "datmrgNH3.rds")
  } 
  
  maxv = round(max(coor$concentration))  
  minv = round(min(coor$concentration)) 
  print(c(maxv, minv))
  
  # ggmap uses lon/lat, not lat/lon.
  source("./centercal.R")
  center <- centercal(posLat,posLon)
  
  mp <- ggmap(get_map(location = center, zoom=zoomfactor, maptype="satellite")) # "terrain"  "hybrid"  "satellite"
  
  # Color index plot to indicate the concentration along the driving route.    
  title <- if(segflag=="OFF"){
    paste(gas, " concentration along the driving route (", substr(daytime[1],1,2), ":", substr(daytime[1],3,4), ":",
          substr(daytime[1],5,6)," -- ", substr(daytime[length(daytime)],1,2), ":", substr(daytime[length(daytime)],3,4), 
          ":", substr(daytime[length(daytime)],5,6), ", May ", k, ")", sep="")
  } else if(segflag=="ON"){
    paste(gas, " concentration along the driving route (", substr(daytime[1],1,2), ":", substr(daytime[1],3,4), ":",
          substr(daytime[1],5,6)," -- ", substr(endSeg,1,2), ":", substr(endSeg,3,4), 
          ":", substr(endSeg,5,6), ", May ", k, ")", sep="")
  } else if(segflag=="22"){
    paste(gas, " concentration along the driving route (", substr(endSeg,1,2), ":", substr(endSeg,3,4), 
          ":", substr(endSeg,5,6)," -- ", substr(daytime[length(daytime)],1,2), ":", substr(daytime[length(daytime)],3,4), 
          ":", substr(daytime[length(daytime)],5,6), ", May ", k, ")", sep="")
  }
  
  if(sizer == 'OFF') {
    mp + ggtitle(title) +   
      geom_point(aes(x = longitude, y = latitude, colour = concentration ), data = coor)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  } else if(sizer == 'ON') {
    mp + ggtitle(title) +   
      geom_point(aes(x = longitude, y = latitude, colour = concentration, size=size ), data = coor)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  }
  
}

# Test code  (k,filename,sizer="OFF",zoomfactor=14,width=10,segflag="OFF",endSeg="141618",mon=5)
conCen_alk2(8,'Butane05_d05_1605081518_0',"ON",14)
conCen_alk2(8,'Butane_insb8_1605081622_0',"ON",12)
conCen_alk2(8,'Butane_insb8_1605081539_0',"ON",12)
conCen_alk2(8,'C3H6_d05_1605081420_0',"ON",14)
conCen_alk2(8,'butadiene_d05_1605081420_0',"ON",14)

conCen_alk2(8,'butadiene_d05_1605081518_0',"ON",14)
conCen_alk2(8,'C3H6_d05_1605081518_0',"ON",14)
conCen_alk2(8,'Butane05_d05_1605081518_0',"ON",14)

conCen_alk(8,'butadiene_d05_1605081338_0',"ON",14,"ON",seg14_2)
conCen_alk2(8,'butadiene_d05_1605081338_0',"ON",14,7,"ON",seg14_2)
conCen_alk2(8,'Butane05_d05_1605081338_0',"ON",14,9,"ON",seg14_2)
conCen_alk2(8,'Butane05_d05_1605081338_0',"ON",14,9,"22",seg14_2)
conCen_alk2(8,'C3H6_d05_1605081338_0',"ON",14,9,"ON",seg14_2)
conCen_alk2(8,'C3H6_d05_1605081338_0',"ON",14,9,"22",seg14_2)

conCen_alk2(8,'Butane_insb8_1605081314_0',"ON",14)

conCen_alk2(8,'Butane_insb8_1605081250_0',"ON",14)

conCen_alk2(8,'Butane_insb8_1605081230_0',"ON",13)

conCen_alk2(8,'Butane_insb8_1605081539_0',"ON",12,9,"ON",seg16_1)
conCen_alk2(8,'Butane_insb8_1605081539_0',"ON",12,9,"22",seg16_1)
conCen_alk2(8,'Butane_insb8_1605081622_0',"ON",12)
