# Om Mani Padme Hum !
# HCHOa concentration plot along the driving route at Tianjing or Beijing in May or June. A subset data 
# for HCHO plotting might be generated, depending on whether or not a subset within each measurement is required. 
# 2016-12-13 1st built, 2017-02-02 2nd modified, 2017-05-03, 3rd modified, 2017-06-07, 4th modified, Weihua Wang. 
# filename like this format "085638".
# k is the specific day.
# width is the spike filter width.
# segflag: If "ON", Seg is a selected time instant.
# Seg: a selected time instant, e.g."141618" 
# most zoomin factor is good at 13, could be 11, 12, or 14.
# mon is the specific month, should be 5 (May) or 6 (June).
# ch="T" selects "Tianjing", ch="B" selects "Beijing".
# size="ON" will turn on the sizer

# Default setting, e.g: filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",endSeg="141618", mon=5, ch="T"

HCHOaconcplot <- function(filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T") {
  
  library(ggplot2)
  library(ggmap) 
  source("./acquireCoor.R")
  source("./maptitle.R")
  source("./centercal.R")
  source("./dirpath.R")
  
  #filename = "154013"; zoom=11; k=8; mon=5; ch="T"; sizer="ON"; segflag="OFF"  # for debugging purpose 
  
  cooR <- acquireCoor(mon,k,ch)             # No outliers.
  cooR$time[1]                       # first time element
  cooR$time[dim(cooR)[1]]            # last time element  
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
  #t_st
  t_ed <- which(dat$Daytime == time_ed)  # find which row has daytime of end measurement
  #t_ed
  
  # The spikes should be detected and rectified here. Modified on 2017-06-07
  source("./spikesfromRMS.R")
  idxVec <- spikesfromRMS(filename,"OFF",k,width)
  #idxVec
  
  source("./spikesReplaced.R")
  #plot(dat$TimeFromStart,dat$HCHOaconc,type='l',col='blue')
  dat$HCHOaconc <- spikesReplaced(dat$HCHOaconc, idxVec)
  #lines(dat$TimeFromStart,dat$HCHOaconc,type='l',col='red')
  
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
  
  df <- cooR[(cooR$time %in% dat[t_st:t_ed, 2]),]   
  
  datmrg <- merge.data.frame(df, concHCHOa, by.x = "time", by.y = "Daytime")  # datmrg means merged data based on common time instant, modified on 2017-02-02
  
  maxv = round(max(datmrg$HCHOaconc))  
  print( paste("Max conc. is ", as.character(maxv), sep="") )
  minv = round(min(datmrg$HCHOaconc)) 
  print( paste("Min conc. is ", as.character(minv), sep="") )
  
  # set concentration that less or equal to 0 to 0.001, for logarithmic calculation purpose.
  #datmrg$NO2aconc[datmrg$NO2aconc <= 0] <- 0.001                             # modified on 2017-02-02
  
  datmrg$HCHOaconc[datmrg$HCHOaconc < 0.01 | datmrg$HCHOaconc > 38] <- 0.01   # modified on 2017-03-02/2017-04-12
  
  # source("./spikesfromRMS.R")
  # idxVec <- spikesfromRMS(filename,k=8,mon=5,ch="T")
  # idxVec
  # source("./spikesReplaced.R")
  # plot(datmrg$time,datmrg$HCHOaconc,type='l')
  # datmrg$HCHOaconc <- spikesReplaced(datmrg$HCHOaconc, idxVec)

  datmrg$size <- datmrg$HCHOaconc
  
  saveRDS(datmrg, "datmrgHCHOa.rds")
  
  center <- centercal(datmrg$latitude, datmrg$longitude)  # modified on 2017-02-02
  
  mp <- ggmap( get_map(location = center, zoom=zoom, maptype="satellite") )  
  
  title <- maptitle("HCHO", datmrg$time, k, ch, mon)             # modified on 2017-02-02
  
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

# Test Code  (filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T")
HCHOaconcplot("085638",11)  # 16-05-08 
HCHOaconcplot("085638",11,,"ON")
HCHOaconcplot("092813",12)
HCHOaconcplot("092813",12,,"ON")
HCHOaconcplot("103526",12)
HCHOaconcplot("103526",12,,"ON",8,"22",seg11)

HCHOaconcplot("122014")
HCHOaconcplot("122014",13,,"ON")
HCHOaconcplot("122014",13,,'ON',8,"ON",seg12_2)
HCHOaconcplot("122014",14,,'ON',8,"22",seg12_1)

HCHOaconcplot("131424",14)
HCHOaconcplot("131424",14,,"ON")

HCHOaconcplot("133927",14)
HCHOaconcplot("133927",14,,"ON")

HCHOaconcplot("142100")
HCHOaconcplot("142100",13,,"ON")
HCHOaconcplot("142100",14,,'ON',8,"ON",seg15_1)

HCHOaconcplot("142100",14,,'ON',8,"22",seg15_2)

HCHOaconcplot("154013",12)
HCHOaconcplot("154013",12,,"ON")
HCHOaconcplot("154013",12,,'ON',8,"ON",seg16_1)
HCHOaconcplot("154013",12,,'ON',8,"22",seg16_2)

