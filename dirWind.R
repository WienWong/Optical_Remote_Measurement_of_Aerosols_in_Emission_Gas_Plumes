
dirWind <- function(filename, t1, t2) {

  # 2017-05-08, 1st built, wind direction in average
  # 2017-05-10, 2nd visit, calculated under John's suggestion
  # 2017-05-16, 3rd visit, throw away all 0s first, then subset the data set.
  # filename contains the wind meter records, for example, "CR200_160508.dat"
  # t1 and t2 are string format of beginning and ending time instant of one measurement, 
  # for example, t1 = "154010", t2 = "162010"
  
  # debugging code
  # filename="CR200_160513.dat"; t1 = "095406"; t2 = "100405" 
  # t1 = "154010"; t2 = "162010"; filename="CR200_160508.dat"
  source('./strVecfromtimeStrvec.R')
  
  pth = './Octave/flameDOAS/Wind/'
  wd <- read.csv( paste(pth, filename, sep=""), header=F)
  names(wd) <- c("TIME", "RECORD", "WS_ms_S_WVT", "WindDir_D1_WVT", "WindDir_SD1_WVT", "Volt") # rename the column

  wd <- wd[c(5:dim(wd)[1]), ]    # extract useful time series, skippig the first 4 unrelated rows
  
  # One case is,
  # row index 5 : 2016-05-11 17:11:00 
  # row index 6 : 2016-05-13 09:41:00
  # If run data subset directly, NaN will be produced. Thus, the other day info needs to be throw away.
  # Idea is throw away all 0s first, then subset the data set.
  dat <- subset(wd, select = c(TIME, WS_ms_S_WVT, WindDir_D1_WVT, WindDir_SD1_WVT) )
  
  # Go through each row and determine if a value is zero. Because I see some 0s at some time instants. Produce logic values
  row_sub = apply(dat, 1, function(row) all(row !=0 ))
  # Subset as usual
  dat <- dat[row_sub, ]
  
  TVec <- substr(dat$TIME, 12,19) # extract time and throw date info
  TVchar <- strVecfromtimeStrvec(TVec) # convert to character vector
  TVnum <- as.numeric(TVchar)
  dat$TIME <- TVnum
  # Subset data frame according to beg and end of each measurement time period.
  dt <- subset(dat, TVnum >= as.numeric(t1) & TVnum <= as.numeric(t2), select = c(TIME, WS_ms_S_WVT, WindDir_D1_WVT) )
  
  #class(dat$WindDir_D1_WVT) # gives 'factor' class
  # Convert from 'factor' to 'numeric'
  Dir <- suppressWarnings( as.numeric(levels(dt$WindDir_D1_WVT))[dt$WindDir_D1_WVT] )
  Spd <- suppressWarnings( as.numeric(levels(dt$WS_ms_S_WVT))[dt$WS_ms_S_WVT] )
  
  # # Mean value of the wind direction in degree
  # 2017-05-08, 1st built, wind direction in average
  # dir <- mean(Dir)
  
  # 2017-05-10, 2nd visit, calculated under John's suggestion
  ##sin(90*pi/180) # must convert to radians because R use radians in Sin, Cos functions.
  Wsx <- Spd*sin(Dir*pi/180)  # A * B : Element-wise multiplication
  Wsy <- Spd*cos(Dir*pi/180)
  
  Mx = mean(Wsx)
  My = mean(Wsy)
  
  # mod( (atan2(Mx, My) in degrees), 360) ## modulo in R is %%, 
  dir <- (atan2(Mx, My)*180/pi) %% 360  # same as atan2(Mx, My)%%(2*pi) *180/pi
  
  return(dir)
  }

# Test code
# dirWind('CR200_160508.dat', "154010", "162010") # 202.676249
