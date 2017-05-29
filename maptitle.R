
maptitle <- function(gas,timev,k,S,yue) {
  # Om Mani Padme Hum !
  # Creasy if-selection branches! TOT
  # 2017-02-02, 1st built, Wang Weihua
  # To avoid time expression look like this '91:35:6' for instance, thus check if first two substrings of daytime greater than 24.
  # gas  : SO2, NO2, HCHO, IR, IRGas
  # timev: a time vector 
  # k    : specific date
  # S    : should be "T" for Tianjing or "B" for Beijing
  # yue  : specific month, should be May or June.
  
  if (gas=="SO2") {
    if (S=="T") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("SO2 concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("SO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("SO2 concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    } else if (S=="B") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("SO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("SO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("SO2 concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
        
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("SO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("SO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("SO2 concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    }
  } else if (gas=="NO2") {
    if (S=="T") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("NO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("NO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("NO2 concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("NO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("NO2 concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("NO2 concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    } else if (S=="B") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("NO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("NO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("NO2 concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
        
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("NO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("NO2 concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("NO2 concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    }
  } else if (gas=="HCHO") {
    if (S=="T") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("HCHO concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("HCHO concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("HCHO concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("HCHO concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("HCHO concentration along the driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("HCHO concentration along the driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    } else if (S=="B") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("HCHO concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("HCHO concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("HCHO concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
        
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("HCHO concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("HCHO concentration along the driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("HCHO concentration along the driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    }
  } else if (gas=="IR") {
    if (S=="T") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("Intensity Ratio of 340/675 in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("Intensity Ratio of 340/675 in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("Intensity Ratio of 340/675 in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("Intensity Ratio of 340/675 in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("Intensity Ratio of 340/675 in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("Intensity Ratio of 340/675 in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    } else if (S=="B") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("Intensity Ratio of 340/675 in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("Intensity Ratio of 340/675 in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("Intensity Ratio of 340/675 in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
        
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("Intensity Ratio of 340/675 in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("Intensity Ratio of 340/675 in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("Intensity Ratio of 340/675 in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    }
  } else if (gas=="IRGas") {
    if (S=="T") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Tianjing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    } else if (S=="B") {
      if (yue==5) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", May ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }else {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", May ", k, ", 2016)", sep="")
        }
        
      } else if (yue==6) {
        if ( substr(timev[1],1,2) > 24 & substr(timev[length(timev)],1,2) > 24){
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- 0", substr(timev[length(timev)],1,1), ":", 
                         substr(timev[length(timev)],2,3), ":", substr(timev[length(timev)],4,5), ", June ", k, ", 2016)", sep="")
        } else if (substr(timev[1],1,2) > 24) {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing ( 0", substr(timev[1],1,1), ":", substr(timev[1],2,3), 
                         ":", substr(timev[1],4,5), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }else {
          title <- paste("IR of 340/675, conc. of selected gas species along driving route in Beijing (", substr(timev[1],1,2), ":", substr(timev[1],3,4), 
                         ":", substr(timev[1],5,6), " -- ", substr(timev[length(timev)],1,2), ":", 
                         substr(timev[length(timev)],3,4), ":", substr(timev[length(timev)],5,6), ", June ", k, ", 2016)", sep="")
        }
      }
    }
  }
  
}
