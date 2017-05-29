# Om Mani Padme Hum !
# HCHOa concentration plot along the driving route at Tianjing or Beijing in May or June, 2016. A subset data 
# for HCHO plotting might be generated, depending on whether or not a subset within each measurement is required. 
# 2016-12-13 1st built, 2017-02-02 2nd modified, 2017-05-03, 3rd visit, Weihua Wang. 
# filename like this format "085638".
# k is the specific day.
# segflag: If "ON", Seg is a selected time instant.
# Seg: a selected time instant, e.g."141618" 
# most zoomin factor is good at 13, could be 11, 12, or 14.
# mon is the specific month, should be 5 (May) or 6 (June).
# ch="T" selects "Tianjing", ch="B" selects "Beijing".
# size="ON" will turn on the sizer
# Default setting, e.g: filename="131424",zoom=13,sizer="OFF",k=8,segflag="OFF",endSeg="141618", mon=5, ch="T"

HCHOaconcplot <- function(filename="131424",zoom=13,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T") {
  
  library(ggplot2)
  library(ggmap) 
  source("./acquireCoor.R")
  source("./maptitle.R")
  source("./centercal.R")
  source("./dirpath.R")
  
  # filename = "085638"; zoom=11; k=8; mon=5; ch="T"; sizer="ON"  # for debugging purpose 
  
  cooR <- acquireCoor(mon,k,ch)          # No outliers.
  cooR$time[1]                           # first time element
  cooR$time[dim(cooR)[1]]                # last time element  
  # or length(cooR$time)
  
  # timeInfo = c("085638", "092813", "103526", "122014", "131424", "133927", "142100", "154013")
  dirpath <- dirpath(mon,k,"doas")
  options(digits=9)                      # extend default digits in numeric value
  
  filepath = paste(dirpath, filename, ".csv", sep = "")
  
  dat = read.csv(filepath)
  # Name each variable                   # modified on 2017-02-02
  names(dat) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                  "SolarZen","Windv","Windd","RMS","HCHOaconc","HCHOfconc","HCHObconc","NO2aconc","NO2fconc","NO2bconc","SO2conc",
                  "O3conc","HCHOeconc","NO2econc","Totalextconc","avgc/molec","Totalextflux","Light")
  
  time_st = dat$Daytime[1]
  time_ed = dat$Daytime[length(dat$Daytime)]
  t_st <- which(dat$Daytime == time_st)  # find which row has daytime at start measurement
  t_st
  t_ed <- which(dat$Daytime == time_ed)  # find which row has daytime of end measurement
  t_ed
  
  # First subset HCHOa conc and daytime, then subset longitude and latitude 
  if(segflag=="OFF"){
    concHCHOa <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(HCHOaconc, Daytime) )
  } else if(segflag=="ON"){
    concHCHOa <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(Seg), select = c(HCHOaconc, Daytime) )
  } else if(segflag=="22"){
    concHCHOa <- subset(dat, Daytime >= as.numeric(Seg) & Daytime <= time_ed, select = c(HCHOaconc, Daytime) ) # modified on 2017-05-04
  } 
  # if(segflag=="OFF"){
  #   concHCHOa <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(HCHOaconc, Daytime) )
  # } else if(segflag=="ON"){
  #   concHCHOa <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(endSeg), select = c(HCHOaconc, Daytime) )
  # }                                                # modified on 2017-05-03
  
  df <- cooR[(cooR$time %in% dat[t_st:t_ed, 2]),]    # nrow(df)
  
  datmrg <- merge.data.frame(df, concHCHOa, by.x = "time", by.y = "Daytime")  # datmrg means merged data based on common time instant, modified on 2017-02-02
  
  # datmrg$size <- datmrg$HCHOaconc  # for sizer 
  
  maxv = round(max(datmrg$HCHOaconc))  
  print( paste("Max conc. is ", as.character(maxv), sep="") )
  minv = round(min(datmrg$HCHOaconc)) 
  print( paste("Min conc. is ", as.character(minv), sep="") )
  
  # set concentration that less or equal to 0 to 0.01, for logarithmic calculation purpose.
  #datmrg$NO2aconc[datmrg$NO2aconc <= 0] <- 0.001         # modified on 2017-02-02
  
  datmrg$HCHOaconc[datmrg$HCHOaconc < 0.01 | datmrg$HCHOaconc > 38] <- 0.01     # modified on 2017-03-02/2017-04-12
  datmrg$size <- datmrg$HCHOaconc
  
  saveRDS(datmrg, "datmrgHCHOa.rds")
  
  center <- centercal(datmrg$latitude, datmrg$longitude)  # modified on 2017-02-02
  
  mp <- ggmap( get_map(location = center, zoom=zoom, maptype="satellite") )  
  
  title <- maptitle("HCHO", datmrg$time, k, ch, mon)      # modified on 2017-02-02
  
  if (sizer=="ON") {
    mp + 
      ggtitle(title) +          # colour = log10(HCHOaconc), size=log10(size) 
      geom_point(aes(x = longitude, y = latitude, colour = HCHOaconc, size=size ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  } else if (sizer=="OFF") {
    mp + 
      ggtitle(title) +          # colour = log10(HCHOaconc)
      geom_point(aes(x = longitude, y = latitude, colour = HCHOaconc ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
  }
  
}

# Test Code  NO2concplot(filename="131424", zoom=13, k=8, mon=5, ch="T", sizer="ON")

HCHOaconcplot("090023",13,"ON",7,"ON",seg1_1)  # 16-05-07


HCHOaconcplot("085638",11)  # 16-05-08 
HCHOaconcplot("085638",11,"ON")
HCHOaconcplot("092813",12)
HCHOaconcplot("092813",12,"ON")
HCHOaconcplot("103526",12)
HCHOaconcplot("103526",12,"ON",8,"22",seg11)

HCHOaconcplot("122014")
HCHOaconcplot("122014",13,"ON")
HCHOaconcplot("122014",13,'ON',8,"ON",seg12_2)
HCHOaconcplot("122014",14,'ON',8,"22",seg12_1)

HCHOaconcplot("131424",14)
HCHOaconcplot("131424",14,"ON")

HCHOaconcplot("133927",14)
HCHOaconcplot("133927",14,"ON")

HCHOaconcplot("142100")
HCHOaconcplot("142100",13,"ON")
HCHOaconcplot("142100",14,'ON',8,"ON",seg15_1)

HCHOaconcplot("142100",14,'ON',8,"22",seg15_2)

HCHOaconcplot("154013",12)
HCHOaconcplot("154013",12,"ON")
HCHOaconcplot("154013",12,'ON',8,"ON",seg16_1)
HCHOaconcplot("154013",12,'ON',8,"22",seg16_2)

#
HCHOaconcplot("084036",14,,10)  # 16-05-10 
HCHOaconcplot("084036",14,"ON",10) 
HCHOaconcplot("090419",11,,10)
HCHOaconcplot("090419",12,"ON",10) # zoonin=11
HCHOaconcplot("104153",11,,10)
HCHOaconcplot("104153",11,"ON",10)
HCHOaconcplot("115632",13,,10)
HCHOaconcplot("115632",13,"ON",10)
HCHOaconcplot("123708",14,,10)
HCHOaconcplot("123708",14,"ON",10,"ON",seg5_1)


HCHOaconcplot("134355",12,,10)
HCHOaconcplot("134355",14,"ON",10,"ON",seg6_1)
HCHOaconcplot("134355",12,"ON",10,"22",seg6_3)

HCHOaconcplot("152415",11,,10)
HCHOaconcplot("152415",11,"ON",10)

HCHOaconcplot("100142",14,"ON",11)  # zoonin=13
HCHOaconcplot("101642",12,"ON",11) 
HCHOaconcplot("110718",11,"ON",11) 
#
HCHOaconcplot("095406",15,"ON",13,"ON",seg2_2)  # 16-05-13
HCHOaconcplot("101805",13,"ON",13)
HCHOaconcplot("103527",13,"ON",13)
HCHOaconcplot("105440",13,"ON",13)
HCHOaconcplot("110654",13,"ON",13)
HCHOaconcplot("112142",13,"ON",13)
HCHOaconcplot("113308",13,"ON",13)
HCHOaconcplot("114707",13,"ON",13)
HCHOaconcplot("120923",13,"ON",13)
HCHOaconcplot("124536",12,"ON",13)
HCHOaconcplot("130927",12,"ON",13)
HCHOaconcplot("133554",12,"ON",13)
HCHOaconcplot("141613",13,"ON",13)
HCHOaconcplot("142924",13,"ON",13,"ON",seg16_1)
HCHOaconcplot("144729",12,"ON",13)  


#
HCHOaconcplot("101420",12,"ON",15)  # 16-05-15
HCHOaconcplot("105151",16,"ON",15)
HCHOaconcplot("113147",13,"ON",15)
HCHOaconcplot("114635",13,"ON",15)
HCHOaconcplot("120123",13,"ON",15)

HCHOaconcplot("121901",13,"ON",15)
HCHOaconcplot("123450",13,"ON",15)
HCHOaconcplot("125436",13,"ON",15,"ON",seg9_1)
HCHOaconcplot("131004",13,"ON",15)
HCHOaconcplot("132329",13,"ON",15,"ON",seg10_2)
HCHOaconcplot("135143",13,"ON",15,"22",seg11_1)
#HCHOaconcplot("135143",13,"ON",15,"ON",seg12_0)
HCHOaconcplot("141058",13,"ON",15,"ON",seg12_1)
HCHOaconcplot("141058",13,"ON",15,"22",seg12_2)
#HCHOaconcplot("141058",13,"ON",15)
HCHOaconcplot("153406",13,"ON",15)
HCHOaconcplot("154425",14,"ON",15)
HCHOaconcplot("155400",13,"ON",15)
HCHOaconcplot("160530",13,"ON",15,"ON",seg16_1)
HCHOaconcplot("161922",13,"ON",15,"ON",seg17_1)

#

HCHOaconcplot("110338",14,"ON",16)
HCHOaconcplot("111225",14,"ON",16)
HCHOaconcplot("112056",14,"ON",16)  # 16-05-16
HCHOaconcplot("112834",14,"ON",16) 
HCHOaconcplot("114047",14,"ON",16)
HCHOaconcplot("121304",14,"ON",16)
HCHOaconcplot("122222",14,"ON",16)
HCHOaconcplot("125022",14,"ON",16)
HCHOaconcplot("125916",14,"ON",16)
HCHOaconcplot("134328",14,"ON",16)
HCHOaconcplot("135740",14,"ON",16)
HCHOaconcplot("141103",14,"ON",16)
HCHOaconcplot("141736",14,"ON",16)
HCHOaconcplot("143040",14,"ON",16)
HCHOaconcplot("144020",14,"ON",16)
HCHOaconcplot("145331",14,"ON",16)
HCHOaconcplot("150209",14,"ON",16)
HCHOaconcplot("151323",14,"ON",16)
HCHOaconcplot("152241",14,"ON",16)
HCHOaconcplot("155222",14,"ON",16)

#
HCHOaconcplot("104314",14,"ON",17)
HCHOaconcplot("104314",14,"ON",17,"ON",seg2_1)
HCHOaconcplot("105541",14,"ON",17)
HCHOaconcplot("110325",14,"ON",17)
HCHOaconcplot("110325",14,"ON",17,"ON",seg4_1)
HCHOaconcplot("113152",14,"ON",17)
HCHOaconcplot("113152",14,"ON",17,"ON",seg6_1)
HCHOaconcplot("115702",14,"ON",17)
HCHOaconcplot("120418",14,"ON",17)
HCHOaconcplot("120418",14,"ON",17,"ON",seg7_1)
HCHOaconcplot("123927",14,"ON",17)
HCHOaconcplot("123927",14,"ON",17,"ON",seg9_1)
HCHOaconcplot("130433",15,"ON",17)
HCHOaconcplot("134752",14,"ON",17)
HCHOaconcplot("134752",14,"ON",17,"ON",seg10_1)
HCHOaconcplot("142537",14,"ON",17)
HCHOaconcplot("143540",13,"ON",17)
HCHOaconcplot("144751",13,"ON",17)
HCHOaconcplot("144751",13,"ON",17,"ON",seg14_1)
HCHOaconcplot("150607",13,"ON",17)
