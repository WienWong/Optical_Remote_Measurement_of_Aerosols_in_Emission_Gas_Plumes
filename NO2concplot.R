# Om Mani Padme Hum !
# NO2 concentration plot along the driving route at Tianjing or Beijing in May or June.A subset data 
# for NO2 plotting might generated, depending on whether or not a subset within each measurement is required. 
# 2017-02-02 1st built, 2017-02-02 2nd modified, 2017-05-03, 3rd visit, Weihua Wang. 
# filename like this format "085638".
# k is the specific day.
# width is the spike filter width.
# segflag: If "ON", Seg is a selected time instant.
# Seg: a selected time instant, e.g."141618" 
# most zoomin factor is good at 13, could be 11, 12, or 14.
# mon is the specific month, should be 5 (May) or 6 (June).
# ch="T" selects "Tianjing", ch="B" selects "Beijing".
# size="ON" will turn on the sizer
# Default setting, e.g: filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",endSeg="141618",mon=5,ch="T"

NO2concplot <- function(filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5) {
  
  library(ggplot2)
  library(ggmap) 
  source("./acquireCoor.R")
  source("./maptitle.R")
  source("./centercal.R")
  source("./dirpath.R")
  
  ch = "T"
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
  #plot(dat$TimeFromStart,dat$NO2aconc,type='l',col='blue')
  dat$NO2aconc <- spikesReplaced(dat$NO2aconc, idxVec)
  #lines(dat$TimeFromStart,dat$NO2aconc,type='l',col='red')
  
  # First subset NO2 conc and daytime, then subset longitude and latitude  
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
  # }                                                  # modified on 2017-05-03
  
  df <- cooR[(cooR$time %in% dat[t_st:t_ed, 2]),]    # nrow(df)

  datmrg <- merge.data.frame(df, concNO2, by.x = "time", by.y = "Daytime")  # datmrg means merged data based on common time instant, modified on 2017-02-02

  maxv = round(max(datmrg$NO2aconc))  
  print( paste("Max conc. is ", as.character(maxv), sep="") )
  minv = round(min(datmrg$NO2aconc)) 
  print( paste("Min conc. is ", as.character(minv), sep="") )
  
  # set concentration that less or equal to 0 to 0.001, for logarithmic calculation purpose.
  # datmrg$NO2aconc[datmrg$NO2aconc <= 0] <- 0.001     # modified on 2017-02-02
  
  datmrg$NO2aconc[datmrg$NO2aconc < 0.01] <- 0.01     # modified on 2017-03-02
  datmrg$size <- datmrg$NO2aconc
  
  saveRDS(datmrg, "datmrgNO2.rds")

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

# Test Code (filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5)
NO2concplot("154013",12,5,'ON',8,"22",seg16_2)

