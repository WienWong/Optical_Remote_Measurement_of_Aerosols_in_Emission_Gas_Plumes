# This function aims to plot out concentration of NH3 or Butane(C4H10) along the driving route in Tianjing. A subset data 
# for NH3 or Butane plotting might be generated, depending on whether or not a subset within each measurement is required. 
# SOF files contain positioning info, thus no need to merge with GPS log file.
# In order to subset time interval within each measurement, the 2nd column of 'daytime' is required  
# 2016-12-08, 1st built, 2017-02-28, 2nd visit, 2017-05-03, 3rd visit, Weihua Wang. 
#
# k is a number specifing the day from 1 to 31.
# filename should be a string like 'butadiene_d05_1605081338_0' or "Butane05_d05_1605081420_0"
# The limit of zoomfactor is 1 to 21, default value is 14.
# segflag: If "ON", endSeg is the last time instant.
# endSeg: the ending time instant, e.g."141618" 
#
# Old name: conCentrationPlot_drivingRouteMod 

conCen_driving1<- function(k,filename,sizer="OFF",zoomfactor=14,segflag="OFF",endSeg="141618"){
  # NH3 or Butane measurements on May kth 2016
  
  library(ggplot2)
  library(ggmap) 
  
  if (0<k & k<10){
    dirpath = "/home/wien/Octave/flameDOAS/GPS_SOF/16050"
    filepath = paste(dirpath, k, "/", filename, ".csv", sep = "")
  } else if (10<=k & k<=31){
    dirpath = "/home/wien/Octave/flameDOAS/GPS_SOF/1605"
    filepath = paste(dirpath, k, "/", filename, ".csv", sep = "")
  }
  
  gas = ""
  if (substr(filename, 1, 3) == "But"){
    gas = "Butane"
  }else if (substr(filename, 1, 3) == "but"){
    gas = "NH3"
  }
  
  dat = read.csv(filepath)
  
  datNArm <- na.omit(dat)  # remove the NA values in the first row, datNArm means NA removed.
  
  options(digits=9)        # extend default digits in numeric value
  
  colnames(datNArm)[2] <- "Daytime"  # Just rename the second column variable's name. Not names(datNArm[,2]) <- "Daytime" 
  # class(datNArm$Daytime) # this is a factor
  colnames(datNArm)[8] <- "Lat"
  colnames(datNArm)[9] <- "Lon"
  colnames(datNArm)[19] <- "conCen"
  
  posLat <- datNArm[,8]    # extract latitude, posLat means position of latitude 
  class(posLat)
  posLon <- datNArm[,9]    # extract longitude 
  conCen <- datNArm[,19]   # extract concentration of NH3 or Butane
  Daytime <- datNArm[,2]
  class(Daytime)
  daytime <- as.vector.factor(Daytime)  # convert a factor to a character vector
  dd<-as.numeric(daytime)
  time_st = dd[1]
  time_ed = dd[length(daytime)]
  
  coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
 
  if(segflag=="OFF"){
    coordinate[, 1] <- posLon        # longitude
    coordinate[, 2] <- posLat        # latitude
    coordinate[, 3] <- conCen        # concentration  
    coor <- data.frame(coordinate)   # need data frame not matrix format
    names(coor) <- c('longitude','latitude','concentration') # add names 
  } else if(segflag=="ON"){
    coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
    coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
    coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(conCen, Daytime) )
    datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
    coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
    coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
    names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-03 
  } else if(segflag=="22"){
    coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
    coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
    coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(conCen, Daytime) )
    datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
    coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
    coor <- coor[,c(2,3,4)]          # Remove 'Daytime' column of factor class.
    names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-04
  }                                       
  
  coor[coor < 0.01 | coor > 1e4] <- 0.01  # modified on 2017-03-02
  
  coor$size <- coor$concentration
  
  if(gas=="Butane"){                      # modified on 2017-03-22
    saveRDS(coor, "datmrgButane.rds")
  } else if(gas=="NH3"){
    saveRDS(coor, "datmrgNH3.rds")
  }
  
  maxv = round(max(coor$concentration))  
  minv = round(min(coor$concentration)) 
  print(c(maxv, minv))
  
  # ggmap uses lon/lat, not lat/lon.
  # center <- c(117.73, 38.94)   # old center pair used for '160508/butadiene_d05_1605081338_0.csv'
  # center <- c( ( max(posLon) + min(posLon) )/2, ( max(posLat) + min(posLat) )/2 ) # center estimation 
  source("./centercal.R")
  center <- centercal(posLat,posLon)
  
  mp <- ggmap(get_map(location = center, zoom=zoomfactor, maptype="satellite")) # "terrain"  "hybrid"  "satellite"
  
  # Color index plot to indicated the concentration along the driving route.    
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

# Testing code
conCen_driving1(8,'Butane_insb8_1605081230_0',"ON",13)
conCen_driving1(8,'Butane_insb8_1605081250_0',"ON",14)

conCen_driving1(8,'Butane_insb8_1605081314_0',"ON",14)

conCen_driving1(8,'butadiene_d05_1605081338_0',"ON",14,"ON","140831")
conCen_driving1(8,'Butane05_d05_1605081338_0',"ON",14,"ON","140831")
#conCentrationPlot_drivingRouteMod(8,'butadiene_d05_1605081338_0',"ON",14,"22",seg14_2)

conCen_driving1(8,'Butane05_d05_1605081518_0',"ON",14)
conCen_driving1(8,'butadiene_d05_1605081518_0',"ON",14)

conCen_driving1(8,'Butane_insb8_1605081539_0',"ON",12,"ON","162021") # "162021" match with other species
conCen_driving1(8,'Butane_insb8_1605081622_0',"ON",12)
conCen_driving1(8,'Butane_insb8_1605081456_0',"ON",13)
conCen_driving1(8,'butadiene_d05_1605081420_0',"ON",14)

#
conCen_driving1(17,'Butane_insb8_1605171042_0',"ON",14,"ON",seg2_1)
conCen_driving1(17,'Butane_insb8_1605171103_0',"ON",14,"ON",seg4_1)
conCen_driving1(17,'Butane_insb8_1605171131_0',"ON",14,"ON",seg6_1)
conCen_driving1(17,'Butane_insb8_1605171204_0',"ON",14,"ON",seg7_1)
conCen_driving1(17,'Butane_insb8_1605171239_0',"ON",14,"ON",seg9_1)
conCen_driving1(17,'Butane_insb8_1605171347_0',"ON",14,"ON",seg10_1)
conCen_driving1(17,'Butane_insb8_1605171425_0',"ON",14)
conCen_driving1(17,'Butane_insb8_1605171435_0',"ON",13)
conCen_driving1(17,'Butane_insb8_1605171447_0',"ON",13,"ON",seg14_1)
conCen_driving1(17,'Butane_insb8_1605171505_0',"ON",13)

#
conCen_driving1(15,'Butane_insb8_1605151410_0',"ON",13,"ON",seg12_1)
conCen_driving1(15,'Butane_insb8_1605151410_0',"ON",13,"22",seg12_2)
conCen_driving1(15,'Butane_insb8_1605151533_0',"ON",13,"ON","154418") # 154418 to match with other species
conCen_driving1(15,'Butane_insb8_1605151553_0',"ON",13) 
conCen_driving1(15,'Butane_insb8_1605151605_0',"ON",13,"ON","161915") # 161915 to match with other species
conCen_driving1(15,'Butane_insb8_1605151605_0',"ON",13,"22","161920") 
conCen_driving1(15,'butadiene_d05_1605151628_0',"ON",13) 
