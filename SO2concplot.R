# Om Mani Padme Hum !
# SO2 concentration plot along the driving route at Tianjing or Beijing in May or June. A subset data 
# for SO2 plotting might generated, depending on whether or not a subset within each measurement is required. 
# 2016-12-13 1st built, 2017-02-02 2nd modified, 2017-05-03, 3rd visit, Weihua Wang. 
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

SO2concplot <- function(filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T") {

  library(ggplot2)
  library(ggmap) 
  source("./acquireCoor.R")
  source("./maptitle.R")
  source("./centercal.R")
  source("./dirpath.R")
  
  # filename = "085638"; zoom=11; width=5; k=8; mon=5; ch="T"  # for debugging purpose 
  
  cooR <- acquireCoor(mon,k,ch)      # No outliers.
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
  #plot(dat$TimeFromStart,dat$SO2conc,type='l',col='blue')
  dat$SO2conc <- spikesReplaced(dat$SO2conc, idxVec)
  #lines(dat$TimeFromStart,dat$SO2conc,type='l',col='red')
  
  # First subset SO2 conc and daytime, then subset longitude and latitude  
  if(segflag=="OFF"){
    concSO2 <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(SO2conc, Daytime) )
  } else if(segflag=="ON"){
    concSO2 <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(Seg), select = c(SO2conc, Daytime) )
  } else if(segflag=="22"){
    concSO2 <- subset(dat, Daytime >= as.numeric(Seg) & Daytime <= time_ed, select = c(SO2conc, Daytime) ) # modified on 2017-05-04
  }
  
  # if(segflag=="OFF"){
  #   concSO2 <- subset(dat, Daytime >= time_st & Daytime <= time_ed, select = c(SO2conc, Daytime) )
  # } else if(segflag=="ON"){
  #   concSO2 <- subset(dat, Daytime >= time_st & Daytime <= as.numeric(endSeg), select = c(SO2conc, Daytime) )
  # }                                                  # modified on 2017-05-03
  
  df <- cooR[(cooR$time %in% dat[t_st:t_ed, 2]),]    # nrow(df)
  
  #### Old version of code #### 
  # select SO2 which shares common time    
  # vec=c()                                        # create an empty vector
  # for ( tt in c( 1:nrow(df) ) ) {
  #   for ( kk in c( 1:nrow(concSO2) ) ) {
  #     if ( df$time[tt] == concSO2$Daytime[kk] ){
  #       vec=c(vec, concSO2$SO2conc[kk])    # append the SO2 concentration 
  #     }
  #   }
  # }
  # xx <- na.omit(vec)  # check if NA exists
  # df$SO2conc <- vec$SO2conc   # forward SO2 concentration to df column named 'SO2conc' 
  # df$size <- vec$SO2conc      # forward SO2 concentration to df column named 'size'
  #### Old version of code #### 
  datmrg <- merge.data.frame(df, concSO2, by.x = "time", by.y = "Daytime")  # datmrg means merged data based on common time instant, modified on 2017-02-02
  
  datmrg$size <- datmrg$SO2conc  # for sizer  
  
  title <- maptitle("SO2", datmrg$time, k, ch, mon)     # put this line of code here thus get the correct time info.  
  
  # Let's add the peak infomation into the df dataframe 
  if(filename=="142100"){   # first create a data frame includes peak info.
    peakInfo <- matrix(c(117.725438, 38.928583, 142649, 1670.7762663956, 1670.7762663956), 
                       nrow=1, ncol=5)
    df_peak <- data.frame(peakInfo)
    names(df_peak) <- c("longitude", "latitude", "time", "SO2conc", "size")
    # To join two data frames (datasets) vertically, use the rbind function. 
    # The two data frames must have the same variables, but they do not have to be in the same order.
    datmrg <- rbind(df_peak,datmrg) 
  }
  
  maxv = round(max(datmrg$SO2conc))  
  print( paste("Max conc. is ", as.character(maxv), sep="") )
  minv = round(min(datmrg$SO2conc)) 
  print( paste("Min conc. is ", as.character(minv), sep="") )
  
  # set concentration that less or equal to 0 to 0.001, for logarithmic calculation purpose.
  # datmrg$SO2conc[datmrg$SO2conc <= 0] <- 0.001     # modified on 2017-02-02
  
  datmrg$SO2conc[datmrg$SO2conc < 0.01] <- 0.01      # modified on 2017-03-01
  
  datmrg$size <- datmrg$SO2conc
  
  saveRDS(datmrg, "datmrgSO2.rds")
  
  #### Old version of code #### 
  # center <- c( ( max(datmrg$longitude) + min(datmrg$longitude) )/2, ( max(datmrg$latitude) + min(datmrg$latitude) )/2 )  # estimate the google map center
  # center   # 117.606392  38.805385
  #### Old version of code #### 
  center <- centercal(datmrg$latitude, datmrg$longitude)  # modified on 2017-02-02
  
  #### Old version of code #### 
  # Tianjing.map <- get_map(location = center, zoom=zoom, maptype="satellite")  # "terrain"  "hybrid"  "satellite" or 'tianjing'
  # mp <- ggmap(Tianjing.map)  
  #### Old version of code #### 
  mp <- ggmap( get_map(location = center, zoom=zoom, maptype="satellite") )  
  
  #### Old version of code #### 
  # # To avoid time expression look like this '91:35:6' for instance, thus we need to check if first two substrings of daytime greater than 24.
  # if ( substr(df$time[1],1,2) > 24 & substr(df$time[length(df$time)],1,2) > 24){
  #   title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
  #                  ":", substr(df$time[1],4,5), " -- 0", substr(df$time[length(df$time)],1,1), ":", 
  #                  substr(df$time[length(df$time)],2,3), ":", substr(df$time[length(df$time)],4,5), ", May ", k, ", 2016)", sep="")
  # } else if (substr(df$time[1],1,2) > 24) {
  #   title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
  #                  ":", substr(df$time[1],4,5), " -- ", substr(df$time[length(df$time)],1,2), ":", 
  #                  substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
  # }else {
  #   title <- paste("SO2 concentration along the driving route in Tianjing (", substr(df$time[1],1,2), ":", substr(df$time[1],3,4), 
  #                  ":", substr(df$time[1],5,6), " -- ", substr(df$time[length(df$time)],1,2), ":", 
  #                  substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
  # }
  #### Old version of code #### 
  #title <- maptitle("SO2", datmrg$time, k, ch, mon)  # modified on 2017-02-02  # I put this line of code above
  
  if (sizer=="ON") {
    mp + ggtitle(title) +
      geom_point(aes(x = longitude, y = latitude, colour = SO2conc, size = size ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
      #scale_colour_gradient(low="skyblue", high="firebrick1")  colour = log10(SO2conc), size=log10(size)
  } else if (sizer=="OFF") {
    mp + ggtitle(title) +   
      geom_point(aes(x = longitude, y = latitude, colour = SO2conc ), data = datmrg)  +
      scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )
      #scale_colour_gradient(low="skyblue", high="firebrick1")   log10(SO2conc)
  }

}

# Test Code  SO2concplot(filename="131424",zoom=13,width=5,sizer="OFF",k=8,segflag="OFF",Seg="141618",mon=5,ch="T")

SO2concplot("085638",11)       # 16-05-08 
SO2concplot("085638",11,,'ON')  
SO2concplot("092813",12)
SO2concplot("092813",12,,'ON')
SO2concplot("103526",12)
SO2concplot("103526",12,,'ON',8,"22",seg11)
SO2concplot("122014")
SO2concplot("122014",13,,'ON')
SO2concplot("122014",13,,'ON',8,"ON",seg12_2)
SO2concplot("122014",14,,'ON',8,"22",seg12_1)

SO2concplot("131424",14)
SO2concplot("131424",14,,'ON')

SO2concplot("133927",14)
SO2concplot("133927",14,,'ON')
SO2concplot("133927",14,,'ON',8)

SO2concplot("142100")
SO2concplot("142100",13,,'ON')
SO2concplot("142100",14,,'ON',8,"ON",seg15_1)

SO2concplot("142100",14,,'ON',8,"22",seg15_2)

SO2concplot("154013",12)
SO2concplot("154013",12,,'ON')
SO2concplot("154013",12,,'ON',8,"ON",seg16_1)
SO2concplot("154013",12,,'ON',8,"22",seg16_2)

######################################## Old old version of code ########################################


dirpath = "/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/DOASeval_160508_"
timeInfo2 = c("085638", "092813", "122014", "131424", "133927")
coor0508 <- acquireCoor(8)
filepath = paste(dirpath, timeInfo2[5], ".csv", sep = "")
k=8

dat = read.csv(filepath)
time_st = dat$X.daytime.[1]
time_ed = dat$X.daytime.[length(dat$X.daytime.)]
t_st <- which(dat$X.daytime. == time_st)  # this will find which row has daytime of 142100
t_st
t_ed <- which(dat$X.daytime. == time_ed)  # this will find which row has daytime of 154001
t_ed
#concSO2 <- dat[t_st:length(dat$X.daytime.) ,23] # extract SO2 at 23th column of dat from t_st to t_ed
# However, this is not what I want since GPS time is not continued.
length(dat$X.daytime.)
nrow(dat)

timeinter <- dat[t_st:t_ed, 2]    # extract daytime at 2th column of dat from t_st to t_ed
timeinter[1]       # check if correct extracted
tail(timeinter,1)  # check if correct extracted

dat[t_st, 2]
dat[t_ed, 2]
df <- coor0508[(coor0508$time %in% dat[t_st:t_ed, 2]),]
## Below double for loop gives the same result as above one line of code.
# vecLon=c()
# for ( t in c( 1:nrow(coor0508) ) ) {
#   for ( k in c( 1:length(timeinter) ) ) {
#     if ( coor0508$time[t] == timeinter[k] ){
#       vecLon=c(vecLon, coor0508$longitude[t])
#     }
#   }
# }

# This is an example: df <- df[!(df$Number %in% remove$Number),]
# %in% will return TRUE if df$Number is in the vector remove. 
# Since we want the complement, we negate the set first with !.

concSO2 <- dat[ , 23]

df$time[1]
tail(df$time,1)
concSO2[1]       # check the first element
tail(concSO2,1)  # check the last element

df$SO2conc <- concSO2  # add SO2 concentration into df 
df$size <- concSO2     # add sizer of SO2 concentration into df 
maxv = round(max(df$SO2conc))  
maxv
minv = round(min(df$SO2conc)) 
minv 

center <- c( ( max(df$longitude) + min(df$longitude) )/2, ( max(df$latitude) + min(df$latitude) )/2 )  # estimate the google map center
center
zoom=14
Tianjing.map <- get_map(location = center, zoom=zoom, maptype="satellite")  # "terrain"  "hybrid"  "satellite" or 'tianjing'
mp <- ggmap(Tianjing.map)  

# To avoid time expression look like this '91:35:6' for instance, thus we need to check if first two substrings of daytime greater than 24.
if ( substr(df$time[1],1,2) > 24 & substr(df$time[length(df$time)],1,2) > 24){
  title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
                 ":", substr(df$time[1],4,5), " -- 0", substr(df$time[length(df$time)],1,1), ":", 
                 substr(df$time[length(df$time)],2,3), ":", substr(df$time[length(df$time)],4,5), ", May ", k, ", 2016)", sep="")
} else if (substr(df$time[1],1,2) > 24) {
  title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
                 ":", substr(df$time[1],4,5), " -- ", substr(df$time[length(df$time)],1,2), ":", 
                 substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
}else {
  title <- paste("SO2 concentration along the driving route in Tianjing (", substr(df$time[1],1,2), ":", substr(df$time[1],3,4), 
                 ":", substr(df$time[1],5,6), " -- ", substr(df$time[length(df$time)],1,2), ":", 
                 substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
}

mp + 
  ggtitle(title) +   
  geom_point(aes(x = longitude, y = latitude, colour = SO2conc, size=size ), data = df)  +
  scale_colour_gradient(low="gray", high="red")  # rainbow(7)  



### plot the peak


timeInfo = c("085638", "092813", "103526", "122014", "131424", "133927", "142100", "154013")
dirpath = "/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/DOASeval_160508_"

### plot "142100" file including the peak.

filepath = paste(dirpath, timeInfo[7], ".csv", sep = "")

dat = read.csv(filepath)

measurement_t = c(142100, 154001);
t_st <- which(dat$X.daytime. == measurement_t[1])  # this will find which row has daytime of 142100
t_st
t_ed <- which(dat$X.daytime. == measurement_t[2])  # this will find which row has daytime of 154001
t_ed

# first subset SO2/time, then subset lon and lat
concSO2 <- subset(dat, X.daytime. >= measurement_t[1] & X.daytime. <= measurement_t[2], select = c(X.conc.SO2c.., X.daytime.) )
k=8
coor0508 <- acquireCoor(5,k,'T')  # No outliers.
df <- coor0508[(coor0508$time %in% dat[t_st:t_ed, 2]),]

nrow(df)
vec=c()
for ( tt in c( 1:nrow(df) ) ) {
  for ( kk in c( 1:nrow(concSO2) ) ) {
    if ( df$time[tt] == concSO2$X.daytime.[kk] ){
      vec=c(vec, concSO2$X.conc.SO2c..[kk])
    }
  }
}
xx <- na.omit(vec)
df$SO2conc <- vec
df$size <- vec
coor0508[17321,]  # This is the row at which the SO2 concentration peak occurs
# Let's add the peak infomation into the df dataframe, first let's create a data frame includes peak info.
peakInfo <- matrix(c(117.725438, 38.928583, 142649, 1670.7762663956, 1670.7762663956), 
                   nrow=1, ncol=5)
df_peak <- data.frame(peakInfo)
names(df_peak) <- c("longitude", "latitude", "time", "SO2conc", "size")
# To join two data frames (datasets) vertically, use the rbind function. 
# The two data frames must have the same variables, but they do not have to be in the same order.
df2 <- rbind(df, df_peak) 
round(max(df$SO2conc))
maxv = round(max(df2$SO2conc))  
maxv
minv = round(min(df$SO2conc)) 
minv 

center <- c( ( max(df2$longitude) + min(df2$longitude) )/2, ( max(df2$latitude) + min(df2$latitude) )/2 )  # estimate the google map center
center
zoom=13
Tianjing.map <- get_map(location = center, zoom=zoom, maptype="satellite")  # "terrain"  "hybrid"  "satellite" or 'tianjing'
mp <- ggmap(Tianjing.map)  

# To avoid time expression look like this '91:35:6' for instance, thus we need to check if first two substrings of daytime greater than 24.
if ( substr(df$time[1],1,2) > 24 & substr(df$time[length(df$time)],1,2) > 24){
  title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
                 ":", substr(df$time[1],4,5), " -- 0", substr(df$time[length(df2$time)],1,1), ":", 
                 substr(df$time[length(df$time)],2,3), ":", substr(df$time[length(df$time)],4,5), ", May ", k, ", 2016)", sep="")
} else if (substr(df$time[1],1,2) > 24) {
  title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(df$time[1],1,1), ":", substr(df$time[1],2,3), 
                 ":", substr(df$time[1],4,5), " -- ", substr(df$time[length(df$time)],1,2), ":", 
                 substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
}else {
  title <- paste("SO2 concentration along the driving route in Tianjing (", substr(df$time[1],1,2), ":", substr(df$time[1],3,4), 
                 ":", substr(df$time[1],5,6), " -- ", substr(df$time[length(df$time)],1,2), ":", 
                 substr(df$time[length(df$time)],3,4), ":", substr(df$time[length(df$time)],5,6), ", May ", k, ", 2016)", sep="")
}

mp + 
  ggtitle(title) +   
  geom_point(aes(x = longitude, y = latitude, colour = SO2conc, size=size ), data = df2)  +
  scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )

mp + 
  ggtitle(title) +   
  geom_point(aes(x = longitude, y = latitude, colour = SO2conc ), data = df2)  +
  scale_color_gradientn( colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red") )

#scale_colour_gradient(low="gray", high="red")  
