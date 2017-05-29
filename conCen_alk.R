conCen_alk<- function(k,filename,sizer="OFF",zoomfactor=14,segflag="OFF",endSeg="141618"){

  # NH3 or Alkene (includes C2H4-Ethylene, C3H6-Propene, C4H6-Butadiene) or Alkane (C3H8-Propane,
  # C4H10-Butane, C8H18-Octane) concentration measurements on May kth 2016
  # 2016-05-12, 1st built, Weihua Wang. 
  # k is a number specifing the day from 1 to 31.
  # filename should be a string like 'butadiene_d05_1605081338_0' or "Butane05_d05_1605081420_0"
  # or 'C3H6_d05_1605151309_0'
  # The limit of zoomfactor is 1 to 21, default value is 14.
  # segflag: If "ON", endSeg is the last time instant. 
  # endSeg: the ending time instant, e.g."141618". It is used to subset the data
  
  library(ggplot2)
  library(ggmap) 
  
  # Debug arguments
  # filename='Butane_insb8_1605151351_0'; k=15; endSeg="135443"; zoomfactor=14;
  
  if (0<k & k<10){
    dirpath = "/home/wien/Octave/flameDOAS/GPS_SOF/16050"
    filepath = paste(dirpath, k, "/", filename, ".csv", sep = "")
  } else if (10<=k & k<=31){
    dirpath = "/home/wien/Octave/flameDOAS/GPS_SOF/1605"
    filepath = paste(dirpath, k, "/", filename, ".csv", sep = "")
  }
  
  dat = read.csv(filepath)
  
  datNArm <- na.omit(dat)  # remove the NA values in the first row, datNArm means NA removed.
  
  options(digits=9)        # extend default digits in numeric value
  
  gas = "";
  if(substr(filename, 1, 3) == "But"){
    gas = "Alkane"
    colnames(datNArm)[2] <- "Daytime"    # Rename the second column variable. Not names(datNArm[,2]) <- "Daytime" because class(datNArm$Daytime) is a factor
    colnames(datNArm)[8] <- "Lat"
    colnames(datNArm)[9] <- "Lon"
    colnames(datNArm)[21] <- "TotalConc" # Total Concentration
    Daytime <- datNArm[,2]
    posLat <- datNArm[,8]                # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]                # extract longitude 
    conCen <- datNArm[,21]               # extract Total Alkane concentration
    daytime <- as.vector.factor(Daytime) # convert a factor to a character vector
    dd<-as.numeric(daytime)
    time_st = dd[1]
    time_ed = dd[length(daytime)]
    
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
    
    if(segflag=="OFF"){
      coordinate[, 1] <- posLon          # longitude
      coordinate[, 2] <- posLat          # latitude
      coordinate[, 3] <- conCen          # concentration  
      coor <- data.frame(coordinate)     # need data frame not matrix format
      names(coor) <- c('longitude','latitude','concentration') # add names 
    } else if(segflag=="ON"){
      coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(TotalConc, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]            # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-03 
    } else if(segflag=="22"){
      coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(TotalConc, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]            # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-04
    } 
    
    coor[coor < 0.01 | coor > 1e4] <- 0.01  # remove outliers modified on 2017-03-02
    
    coor$size <- coor$concentration         # sizer for emphasis plotting
    
    saveRDS(coor, "datmrgAlkane.rds")
    
  } else if(substr(filename, 1, 3) == "but"){
    gas = "Alkene"
    colnames(datNArm)[2] <- "Daytime"  
    colnames(datNArm)[8] <- "Lat"
    colnames(datNArm)[9] <- "Lon"
    colnames(datNArm)[18] <- "C2H4conc"
    colnames(datNArm)[20] <- "C3H6conc"
    colnames(datNArm)[21] <- "C4H6conc"     # Butadiene Concentration
    
    posLat <- datNArm[,8]                   # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]                   # extract longitude 
    conCen <- datNArm[,18]+datNArm[,20]+datNArm[,21] # extract sum concentration of C2H4,C3H6,C4H6 
    #class(conCen)  # "numeric"
    datNArm$conCen <- conCen # add one column variable for subset 
    #class(datNArm$conCen)
    Daytime <- datNArm[,2]
    #class(Daytime) # "factor"
    daytime <- as.vector.factor(Daytime)    # convert a factor to a character vector
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
    
    saveRDS(coor, "datmrgAlkene.rds")
    
  } else if(substr(filename, 1, 3) == "C3H"){
    gas = "NH3"
    colnames(datNArm)[2] <- "Daytime"  
    colnames(datNArm)[8] <- "Lat"
    colnames(datNArm)[9] <- "Lon"
    colnames(datNArm)[19] <- "conCen" # NH3
    Daytime <- datNArm[,2]
    posLat <- datNArm[,8]             # extract latitude, posLat means position of latitude 
    posLon <- datNArm[,9]             # extract longitude 
    conCen <- datNArm[,19]            # extract concentration of NH3 or Butane
    daytime <- as.vector.factor(Daytime)  # convert a factor to a character vector
    dd<-as.numeric(daytime)
    time_st = dd[1]
    time_ed = dd[length(daytime)]
    
    coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
    
    if(segflag=="OFF"){
      coordinate[, 1] <- posLon       # longitude
      coordinate[, 2] <- posLat       # latitude
      coordinate[, 3] <- conCen       # concentration  
      coor <- data.frame(coordinate)  # need data frame not matrix format
      names(coor) <- c('longitude','latitude','concentration') # add names 
    } else if(segflag=="ON"){
      coor1 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= time_st & dd <= as.numeric(endSeg), select = c(conCen, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]         # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-03 
    } else if(segflag=="22"){
      coor1 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lon, Daytime) ) # longitude
      coor2 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(Lat, Daytime) ) # latitude
      coor3 <- subset(datNArm, dd >= as.numeric(endSeg) & dd <= time_ed, select = c(conCen, Daytime) )
      datmrg <- merge.data.frame(coor1, coor2, by.x = "Daytime", by.y = "Daytime")
      coor <- merge.data.frame(datmrg, coor3, by.x = "Daytime", by.y = "Daytime")
      coor <- coor[,c(2,3,4)]         # Remove 'Daytime' column of factor class.
      names(coor) <- c('longitude','latitude','concentration') # add names # modified on 2017-05-04
    } 
    
    coor[coor < 0.01 | coor > 1e4] <- 0.01  # modified on 2017-03-02
    
    coor$size <- coor$concentration
    
    saveRDS(coor, "datmrgNH3.rds")
  } 
  
  maxv = round(max(coor$concentration))  
  minv = round(min(coor$concentration)) 
  print(c(maxv, minv))
  
  # ggmap uses lon/lat, not lat/lon. e.g. c(117.73, 38.94)    
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
conCen_alk(8,'Butane05_d05_1605081518_0',"ON",14)
conCen_alk(8,'Butane_insb8_1605081622_0',"ON",12)
conCen_alk(8,'Butane_insb8_1605081539_0',"ON",12)
conCen_alk(8,'C3H6_d05_1605081420_0',"ON",14)
conCen_alk(8,'butadiene_d05_1605081420_0',"ON",14)
conCen_alk(8,'butadiene_d05_1605081518_0',"ON",14)
conCen_alk(8,'C3H6_d05_1605081518_0',"ON",14)
conCen_alk(8,'Butane05_d05_1605081518_0',"ON",14)

conCen_alk(8,'butadiene_d05_1605081338_0',"ON",14,"ON",seg14_2)
conCen_alk(8,'Butane05_d05_1605081338_0',"ON",14,"ON",seg14_2)
conCen_alk(8,'C3H6_d05_1605081338_0',"ON",14,"ON",seg14_2)

conCen_alk(8,'Butane_insb8_1605081314_0',"ON",14)

conCen_alk(8,'Butane_insb8_1605081250_0',"ON",14)

conCen_alk(8,'Butane_insb8_1605081230_0',"ON",13)

conCen_alk(8,'Butane_insb8_1605081539_0',"ON",12,"ON",seg16_1)

conCen_alk(8,'Butane_insb8_1605081622_0',"ON",12)

conCen_alk(10,'butadiene_d05_1605101232_0',"ON",14,"22",seg5)
conCen_alk(10,'Butane05_d05_1605101232_0',"ON",14,"22",seg5)
conCen_alk(10,'C3H6_d05_1605101232_0',"ON",14,"22",seg5)

conCen_alk(10,'butadiene_d05_1605101249_0',"ON",14,"ON",seg5_3)
conCen_alk(10,'Butane05_d05_1605101249_0',"ON",14,"ON",seg5_3)
conCen_alk(10,'C3H6_d05_1605101249_0',"ON",14,"ON",seg5_3)

conCen_alk(10,'butadiene_d05_1605101249_0',"ON",14,"22",seg5_4)
conCen_alk(10,'Butane05_d05_1605101249_0',"ON",14,"22",seg5_4)
conCen_alk(10,'C3H6_d05_1605101249_0',"ON",14,"22",seg5_4)

conCen_alk(10,'butadiene_d05_1605101343_0',"ON",14)
conCen_alk(10,'Butane05_d05_1605101343_0',"ON",14)
conCen_alk(10,'C3H6_d05_1605101343_0',"ON",14)

conCen_alk(10,'butadiene_d05_1605101420_0',"ON",14)
conCen_alk(10,'Butane05_d05_1605101420_0',"ON",14)
conCen_alk(10,'C3H6_d05_1605101420_0',"ON",14)

conCen_alk(10,'Butane_insb8_1605101446_0',"ON",12)

conCen_alk(10,'Butane_insb8_1605101523_0',"ON",11)

conCen_alk(13,'butadiene_d05_1605130917_0',"ON",14)
conCen_alk(13,'C3H6_d05_1605130917_0',"ON",14)

conCen_alk(13,'butadiene_d05_1605130953_0',"ON",15,"22",seg2)
conCen_alk(13,'C3H6_d05_1605130953_0',"ON",15,"22",seg2)
conCen_alk(13,'Butane05_d05_1605130953_0',"ON",15,"22",seg2)

conCen_alk(13,'butadiene_d05_1605131006_0',"ON",14)
conCen_alk(13,'Butane05_d05_1605131006_0',"ON",14)
conCen_alk(13,'C3H6_d05_1605131006_0',"ON",14)

conCen_alk(13,'butadiene_d05_1605131017_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131017_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131017_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131035_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131035_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131035_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131054_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131054_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131054_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131121_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131121_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131121_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131132_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131132_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131132_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131106_0',"ON",13)
conCen_alk(13,'Butane05_d05_1605131106_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131106_0',"ON",13)

conCen_alk(13,'butadiene_d05_1605131147_0',"ON",13,"ON")
conCen_alk(13,'Butane05_d05_1605131147_0',"ON",13)
conCen_alk(13,'C3H6_d05_1605131147_0',"ON",13)

conCen_alk(15,'C3H6_d05_1605151309_0',"ON",14)

conCen_alk(15,'Butane_insb8_1605151219_0',"ON",13)  # alkane
conCen_alk(15,'Butane_insb8_1605151234_0',"ON",13)  # alkane
conCen_alk(15,'Butane_insb8_1605151254_0',"ON",13,"ON",seg9_1)  # alkane
conCen_alk(15,'butadiene_d05_1605151309_0',"ON",13)  # alkene
conCen_alk(15,'C3H6_d05_1605151309_0',"ON",13)  # NH3
conCen_alk(15,'butadiene_d05_1605151323_0',"ON",13,"ON",seg10_2)  # alkene
conCen_alk(15,'C3H6_d05_1605151323_0',"ON",13,"ON",seg10_2)  # NH3
conCen_alk(15,'Butane_insb8_1605151351_0',"ON",13,"22",seg11_1)  # alkene

conCen_alk(15,'butadiene_d05_1605151309_0',"ON",14,"ON","132155")

conCen_alk(15,'Butane_insb8_1605151410_0',"ON",13,"ON",seg12_1)
conCen_alk(15,'Butane_insb8_1605151410_0',"ON",13,"22",seg12_1)
conCen_alk(15,'Butane_insb8_1605151533_0',"ON",13,"ON","154417") 

conCen_alk(15,'Butane_insb8_1605151553_0',"ON",13)
conCen_alk(15,'Butane_insb8_1605151605_0',"ON",13,"ON",seg16_1)
conCen_alk(15,'Butane_insb8_1605151605_0',"ON",13,"22",seg17)
conCen_alk(15,'C3H6_d05_1605151628_0',"ON",13)
conCen_alk(15,'butadiene_d05_1605151628_0',"ON",13)
