
# Propane(C3H8) or C2H4(Ethylene) conCen along the driving route for Tianjing city
# 2017-05-08, 1st built, Weihua Wang


conCen_driving2 <- function(k,filename,sizer="OFF",zoomfactor=14,segflag="OFF",endSeg="141618"){
  # Propane or C2H4(Ethylene) measurements on May kth 2016
  
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
    gas = "Propane"
  }else if (substr(filename, 1, 3) == "but"){
    gas = "C2H4"
  }
  
  dat = read.csv(filepath)
  
  datNArm <- na.omit(dat)  # remove the NA values in the first row, datNArm means NA removed.
  
  options(digits=9)        # extend default digits in numeric value
  
  colnames(datNArm)[2] <- "Daytime"  # Just rename the second column variable's name. Not names(datNArm[,2]) <- "Daytime" 
  # class(datNArm$Daytime) # this is a factor
  colnames(datNArm)[8] <- "Lat"
  colnames(datNArm)[9] <- "Lon"
  colnames(datNArm)[18] <- "conCen"
  
  posLat <- datNArm[,8]    # extract latitude, posLat means position of latitude 
  class(posLat)
  posLon <- datNArm[,9]    # extract longitude 
  conCen <- datNArm[,18]   # extract concentration of Propane or C2H4(Ethylene)
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
  
  if(gas=="Propane"){                     # modified on 2017-03-22
    saveRDS(coor, "datmrgPropane.rds")
  } else if(gas=="C2H4"){
    saveRDS(coor, "datmrgC2H4.rds")
  }
  
  maxv = round(max(coor$concentration))  
  minv = round(min(coor$concentration)) 
  print(c(maxv, minv))
  
  # ggmap uses lon/lat, not lat/lon.
  # center <- c( ( max(posLon) + min(posLon) )/2, ( max(posLat) + min(posLat) )/2 ) # center estimation 
  source("./centercal.R")
  center <- centercal(posLat,posLon)
  
  mp <- ggmap(get_map(location = center, zoom=zoomfactor, maptype="satellite"))     # "terrain"  "hybrid"  "satellite"
  
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


# Test code
conCen_driving2(15,'Butane_insb8_1605151410_0',"ON",13,"ON",seg12_1)
conCen_driving2(15,'Butane_insb8_1605151410_0',"ON",13,"22",seg12_2)

conCen_driving2(15,'butadiene_d05_1605151628_0',"ON",13) 






