
# Calculate the wind direction  -- Backup code by Weihua Wang
# 2017-05-08, 1st built, wind direction in average
# 2017-05-10, 2nd visit, calculated under John's suggestion

source('./timeStrtoNum.R')
source('./strVecfromtimeStrvec.R')

wd <- read.csv('./Octave/flameDOAS/Wind/CR200_160508.dat', header=F)
names(wd) <- c("TIME", "RECORD", "WS_ms_S_WVT", "WindDir_D1_WVT", "WindDir_SD1_WVT", "Volt")
wd[332,] 
class(wd$TIME)
eg <- substr(wd$TIME[332], 12,19) # 2016-05-08 15:40:00
class(eg)
timeStrtoNum(eg)
dim(wd)
wd <- wd[c(5:dim(wd)[1]), ] # extract useful time series


timevec <- wd$TIME
TV <- substr(timevec, 12,19)
TVnum <- strVecfromtimeStrvec(TV)
t1 = "154010"  #"154000"
t2 = "162010"  #"171200"

dat <- subset(wd, TVnum >= as.numeric(t1) & TVnum <= as.numeric(t2), select = c(TIME, WS_ms_S_WVT, WindDir_D1_WVT) )
##Go through each row and determine if a value is zero
row_sub = apply(dat, 1, function(row) all(row !=0 ))
##Subset as usual
dat <- dat[row_sub,]
class(dat$WindDir_D1_WVT)
dat$WindDir_D1_WVT[1]
# Dir <- strVecfromtimeStrvec(dat$WindDir_D1_WVT)
Dir <- as.numeric(levels(dat$WindDir_D1_WVT))[dat$WindDir_D1_WVT]
Dir[1]
class(Dir)

Spd <- as.numeric(levels(dat$WS_ms_S_WVT))[dat$WS_ms_S_WVT]
Spd[1]
class(Spd)

# Calculation of wind direction
# 2017-05-08, 1st built, wind direction in average
dir <- mean(Dir)
dir    # 202.852477 is the average wind direction of the last time segment. 

# 2017-05-10, 2nd visit, calculated under John's suggestion
##sin(90*pi/180) # must convert to radians because R use radians in Sin, Cos functions.
Wsx <- Spd*sin(Dir*pi/180)  # A * B : Element-wise multiplication
Wsy <- Spd*cos(Dir*pi/180)

Mx = mean(Wsx)
My = mean(Wsy)

# mod( (atan2(Mx, My) in degrees), 360) ## module in r is %%, 
dir <- (atan2(Mx, My)*180/pi) %% 360  # same as atan2(Mx, My)%%(2*pi) *180/pi
dir    # 202.676249 is the updated wind direction of the last time segment. 

