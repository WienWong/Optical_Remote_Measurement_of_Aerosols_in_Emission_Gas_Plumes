# Om Mani Padme Hum !
# NO2 concentration plot along the driving route at Tianjing or Beijing in May or June. A subset data 
# for NO2 plotting might generated, depending on whether or not a subset within each measurement is required. 
# 2017-02-02 1st built, 2017-02-02 2nd modified, 2017-05-03, 3rd visit, Weihua Wang. 
# filename like the format of "085638".
# k is the specific day.
# segflag: If "ON", Seg is a selected time instant.
# Seg: a selected time instant, e.g."141618" 
# Most zoomin factor is good at 13, could be 11, 12, or 14.
# mon is the specific month, should be 5 (May) or 6 (June).
# ch="T" selects "Tianjing", ch="B" selects "Beijing".
# size="ON" will turn on the sizer
# Default setting, e.g: filename="131424",zoom=13,sizer="OFF",k=8,segflag="OFF",endSeg="141618", mon=5, ch="T"

NO2concplot <- function(filename="131424",zoom=13,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T") {
  
  library(ggplot2)
  library(ggmap) 
  source("./acquireCoor.R")
  source("./maptitle.R")
  source("./centercal.R")
  source("./dirpath.R")
  
  cooR <- acquireCoor(mon,k,ch)      # No outliers.
  cooR$time[1]                       # first time element
  cooR$time[dim(cooR)[1]]            # last time element  
  # or length(cooR$time)
  
  # timeInfo = c("085638", "092813", "103526", "122014", "131424", "133927", "142100", "154013")
  dirpath <- dirpath(mon,k,"doas")
  options(digits=9)                  # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  # Name each variable               # modified on 2017-02-02
  names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                  "SolarZen","Windv","Windd","RMS","HCHOaconc","HCHOfconc","HCHObconc","NO2aconc","NO2fconc","NO2bconc","SO2conc",
                  "O3conc","HCHOeconc","NO2econc","Totalextconc","avgc/molec","Totalextflux","Light")
  
  time_st = dat$Daytime[1]
  time_ed = dat$Daytime[length(dat$Daytime)]
  t_st <- which(dat$Daytime == time_st)  # find which row has daytime at start measurement
  t_st
  t_ed <- which(dat$Daytime == time_ed)  # find which row has daytime of end measurement
  t_ed
  
  # First subset NO2 concentration and daytime, then subset longitude and latitude  
  if(segflag=="OFF"){
    concNO2 <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(NO2aconc, Daytime) )
  } else if(segflag=="ON"){
    concNO2 <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(Seg), select = c(NO2aconc, Daytime) )
  } else if(segflag=="22"){
    concNO2 <- subset(dat, Daytime >= as.numeric(Seg) & Daytime <= time_ed, select = c(NO2aconc, Daytime) )
  } 
  
  # if(segflag=="OFF"){
  #   concNO2 <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(NO2aconc, Daytime) )
  # } else if(segflag=="ON"){
  #   concNO2 <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(endSeg), select = c(NO2aconc, Daytime) )
  # }                                                # modified on 2017-05-03
  
  df <- cooR[(cooR$time %in% dat[t_st:t_ed, 2]),]    # nrow(df)

  datmrg <- merge.data.frame(df, concNO2, by.x = "time", by.y = "Daytime")  # datmrg means merged data based on common time instant, modified on 2017-02-02
  
  maxv = round(max(datmrg$NO2aconc))  
  print( paste("Max conc. is ", as.character(maxv), sep="") )
  minv = round(min(datmrg$NO2aconc)) 
  print( paste("Min conc. is ", as.character(minv), sep="") )
  
  # set concentration that less or equal to 0 to 0.001, for logarithmic calculation purpose.
  #datmrg$NO2aconc[datmrg$NO2aconc <= 0] <- 0.001    # modified on 2017-02-02
  
  datmrg$NO2aconc[datmrg$NO2aconc < 0.01] <- 0.01    # The minimum concentration can be recorded ~ 0.01. modified on 2017-03-02
  datmrg$size <- datmrg$NO2aconc
  
  saveRDS(datmrg, "datmrgNO2.rds")                   # Save for combined plotting with IRs and other gas species 

  center <- centercal(datmrg$latitude, datmrg$longitude)  # modified on 2017-02-02

  mp <- ggmap( get_map(location = center, zoom=zoom, maptype="satellite") )  

  title <- maptitle("NO2", datmrg$time, k, ch, mon)       # modified on 2017-02-02
  
  if (sizer=="ON") {
    mp + 
      ggtitle(title) +          # colour = log10(NO2aconc), size=log10(size) 
      geom_point(aes(x = longitude, y = latitude, colour = NO2aconc, size=size ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  } else if (sizer=="OFF") {
    mp + 
      ggtitle(title) +          # colour = log10(NO2aconc)
      geom_point(aes(x = longitude, y = latitude, colour = NO2aconc ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  }
  
}

# Test Code  filename="131424",zoom=13,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T"

NO2concplot("090023",13,"ON",7,"ON",seg1_1)  # 16-05-07

NO2concplot("085638",11)  # 16-05-08 
NO2concplot("085638",11,"ON")
NO2concplot("092813",12)
NO2concplot("092813",12,"ON")
NO2concplot("103526",12)
NO2concplot("103526",12,"ON",8,"22",seg11)

NO2concplot("122014")
NO2concplot("122014",13,"ON")
NO2concplot("122014",13,'ON',8,"ON",seg12_2)
NO2concplot("122014",14,'ON',8,"22",seg12_1)

NO2concplot("131424",14)
NO2concplot("131424",14,"ON")

NO2concplot("133927",14)
NO2concplot("133927",14,"ON")

NO2concplot("142100")
NO2concplot("142100",13,"ON")
NO2concplot("142100",14,'ON',8,"ON",seg15_1)

NO2concplot("142100",14,'ON',8,"22",seg15_2)

NO2concplot("154013",12)
NO2concplot("154013",12,"ON")
NO2concplot("154013",12,'ON',8,"ON",seg16_1)
NO2concplot("154013",12,'ON',8,"22",seg16_2)
#

NO2concplot("084036",14,,10)  # 16-05-10 
NO2concplot("084036",14,"ON",10) 
NO2concplot("090419",11,,10)
NO2concplot("090419",12,"ON",10) # zoonin=11
NO2concplot("104153",11,,10)
NO2concplot("104153",11,"ON",10)
NO2concplot("115632",13,,10)
NO2concplot("115632",13,"ON",10)
NO2concplot("123708",14,,10)
NO2concplot("123708",14,"ON",10,"ON",seg5_1)


NO2concplot("134355",12,,10)
NO2concplot("134355",14,"ON",10,"ON",seg6_1)
NO2concplot("134355",12,"ON",10,"22",seg6_3)


NO2concplot("152415",11,,10)
NO2concplot("152415",11,"ON",10)

NO2concplot("100142",14,"ON",11)  # zoonin=13
NO2concplot("101642",12,"ON",11) 
NO2concplot("110718",11,"ON",11) 
#
NO2concplot("095406",15,"ON",13,"ON",seg2_2)  # 16-05-13
NO2concplot("101805",13,"ON",13)
NO2concplot("103527",13,"ON",13)
NO2concplot("105440",13,"ON",13)
NO2concplot("110654",13,"ON",13)
NO2concplot("112142",13,"ON",13)
NO2concplot("113308",13,"ON",13)
NO2concplot("114707",13,"ON",13)
NO2concplot("120923",13,"ON",13)
NO2concplot("124536",12,"ON",13)
NO2concplot("130927",12,"ON",13)
NO2concplot("133554",12,"ON",13)
NO2concplot("141613",13,"ON",13)
NO2concplot("142924",13,"ON",13,"ON",seg16_1)
NO2concplot("144729",12,"ON",13)  


#
NO2concplot("101420",12,"ON",15)  # 16-05-15
NO2concplot("105151",16,"ON",15)
NO2concplot("113147",13,"ON",15)
NO2concplot("114635",13,"ON",15)
NO2concplot("120123",13,"ON",15)

NO2concplot("121901",13,"ON",15)
NO2concplot("123450",13,"ON",15)
NO2concplot("125436",13,"ON",15,"ON",seg9_1)
NO2concplot("131004",13,"ON",15)
NO2concplot("132329",13,"ON",15,"ON",seg10_2)
NO2concplot("135143",13,"ON",15,"22",seg11_1)
#NO2concplot("135143",13,"ON",15,"ON",seg12_0)
NO2concplot("141058",13,"ON",15,"ON",seg12_1)
NO2concplot("141058",13,"ON",15,"22",seg12_2)
#NO2concplot("141058",13,"ON",15)
NO2concplot("153406",13,"ON",15)
NO2concplot("154425",14,"ON",15)

NO2concplot("155400",13,"ON",15)
NO2concplot("160530",13,"ON",15,"ON",seg16_1)
NO2concplot("161922",13,"ON",15,"ON",seg17_1)

#
NO2concplot("110338",14,"ON",16)
NO2concplot("111225",14,"ON",16)
NO2concplot("112056",14,"ON",16)  # 16-05-16
NO2concplot("112834",14,"ON",16)
NO2concplot("114047",14,"ON",16)
NO2concplot("121304",14,"ON",16)
NO2concplot("122222",14,"ON",16)
NO2concplot("125022",14,"ON",16)
NO2concplot("125916",14,"ON",16)
NO2concplot("134328",14,"ON",16)
NO2concplot("135740",14,"ON",16)
NO2concplot("141103",14,"ON",16)
NO2concplot("141736",14,"ON",16)
NO2concplot("143040",14,"ON",16)
NO2concplot("144020",14,"ON",16)
NO2concplot("145331",14,"ON",16)
NO2concplot("150209",14,"ON",16)
NO2concplot("151323",14,"ON",16)
NO2concplot("152241",14,"ON",16)
NO2concplot("155222",14,"ON",16)

#
NO2concplot("104314",14,"ON",17)
NO2concplot("104314",14,"ON",17,"ON",seg2_1)
NO2concplot("105541",14,"ON",17)
NO2concplot("110325",14,"ON",17)
NO2concplot("110325",14,"ON",17,"ON",seg4_1)
NO2concplot("113152",14,"ON",17)
NO2concplot("113152",14,"ON",17,"ON",seg6_1)
NO2concplot("115702",14,"ON",17)
NO2concplot("120418",14,"ON",17)
NO2concplot("120418",14,"ON",17,"ON",seg7_1)
NO2concplot("123927",14,"ON",17)
NO2concplot("123927",14,"ON",17,"ON",seg9_1)
NO2concplot("130433",15,"ON",17)
NO2concplot("134752",14,"ON",17)
NO2concplot("134752",14,"ON",17,"ON",seg10_1)
NO2concplot("142537",14,"ON",17)
NO2concplot("143540",13,"ON",17)
NO2concplot("144751",13,"ON",17)
NO2concplot("144751",13,"ON",17,"ON",seg14_1)
NO2concplot("150607",13,"ON",17)
