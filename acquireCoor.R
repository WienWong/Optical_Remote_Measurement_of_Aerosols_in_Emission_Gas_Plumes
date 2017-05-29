# Om Mani Padme Hum !
# Acquire GPS coordinates and time information in Tianjing or Beijing Mobeil DOAS measurements between May and June, 2016.
# 2016-12-12, 1st built, 2017-02-02, 2nd modified, by Weihua Wang
# mon and k are the specific month and day, respectively. 
# city can only choose "T" for Tianjing and "B" for Beijing.

acquireCoor <- function(mon,k,city) {
  library(stringr)
  source("./dirpath.R")

  OFFSET = 3                                   # 3 degree on the map is quite large scale enough 
  dirpath <- dirpath(mon,k,"gps")
  filepath <- paste(dirpath, "log.txt", sep = "")
  options(digits=9) 
  
  datRaw = read.table(filepath)
  posLat <- datRaw[,3]                         # extract the latitude
  posLon <- datRaw[,4]                         # extract the longitude
  daytime <- datRaw[,9]                        # extract the day time  
  
  # Read GPS log.txt file for Tianjing or Beijing city
  if (city=="T") {
    
    # A priori coordinate of Tianjing is approx. at c(38.827 Lat,117.44 Lon) thus outliers are outside of a small range around  
    # this coordinates. Why outliers? Because malfunction of the GPS tracker at certain time can record abnormal coordinate pairs! 
    LAT=38.827; LON=117.44
    if ( min(posLon)<LON-OFFSET | max(posLon)>LON+OFFSET | min(posLat)<LAT-OFFSET | max(posLat)>LAT+OFFSET ){
      print("Outliers exist.")
      
      idx = which( posLon<LON-OFFSET | posLon>LON+OFFSET | posLat<LAT-OFFSET | posLat>LAT+OFFSET )
      for (tt in idx) {
        print( paste("Outlier at row: ", as.character(tt), sep = " " ) ) 
      }
      
      coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
      coordinate[, 1] <- posLon                # longitude
      coordinate[, 2] <- posLat                # latitude
      coordinate[, 3] <- daytime               # day time
      coor <- data.frame(coordinate)           # need data frame not matrix format
      names(coor) <- c('longitude','latitude','time') # add names 
      
      remove=toString(idx)
      remove=str_split(remove,", ")[[1]]       # str_split(remove,", ") gives a list, but we have to access the correct list element by [[1]]
      # remove=unlist(str_split(remove,", "))  # this also works fine by using 'stringr' package.
      
      coor <- coor[ !(rownames(coor) %in% remove), ]  # remove outliers
    } else{
      print("No outliers.")
      # If no outliers, then calculate the center estimation directly.
      coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
      coordinate[, 1] <- posLon   
      coordinate[, 2] <- posLat   
      coordinate[, 3] <- daytime  
      coor <- data.frame(coordinate)  
      names(coor) <- c('longitude','latitude','time') 
    }
    
  } else if (city=="B") {
    
    # A priori coordinate of Beijing is approximately at c(39 Lat,115 Lon).
    LAT=39; LON=115
    if ( min(posLon)<LON-OFFSET | max(posLon)>LON+OFFSET | min(posLat)<LAT-OFFSET | max(posLat)>LAT+OFFSET ){
      print("Outliers exist.")
      
      idx = which( posLon<LON-OFFSET | posLon>LON+OFFSET | posLat<LAT-OFFSET | posLat>LAT+OFFSET )
      for (tt in idx) {
        print( paste("Outlier at row: ", as.character(tt), sep = " " ) ) 
      }
      
      coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
      coordinate[, 1] <- posLon   
      coordinate[, 2] <- posLat   
      coordinate[, 3] <- daytime  
      coor <- data.frame(coordinate)  
      names(coor) <- c('longitude','latitude','time') 
      
      remove=toString(idx)
      remove=str_split(remove,", ")[[1]]
      # remove=unlist(str_split(remove,", "))  # this also works fine by using 'stringr' package.
      
      coor <- coor[ !(rownames(coor) %in% remove), ]  
    } else{
      print("No outliers.")
      # If no outliers, then calculate the center estimation directly.
      coordinate <- matrix(data=NA, nrow=length(posLon), ncol=3)
      coordinate[, 1] <- posLon  
      coordinate[, 2] <- posLat   
      coordinate[, 3] <- daytime  
      coor <- data.frame(coordinate)  
      names(coor) <- c('longitude','latitude','time') # add names 
    }
    
  }

  return(coor)
}

# Test code
# coor0508 <- acquireCoor(5,8,"T") 
# coor0602 <- acquireCoor(6,2,"B") 
# coor0513 <- acquireCoor(5,28,"T") 
# coor0605 <- acquireCoor(6,5,"B") 
