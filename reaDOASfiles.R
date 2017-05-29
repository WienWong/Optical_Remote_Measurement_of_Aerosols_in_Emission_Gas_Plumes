
reaDOASfiles <- function(fst,fed) {
  
  # Read a bunch of DOAS files recursively and merge them together in row-wise.
  # The csv files have format of DOASeval_YYMMDD_xxxxxx.csv
  # 2017-03-07, 1st built, Weihua Wang
  # fst: the first csv file index in the listed out file_list
  # fed: the last csv file index in the listed out file_list
  
  setwd("/home/wien/Octave/flameDOAS/DOAS/csvfilesPreliminary/") # filepath where the DOAS files stored
  
  file_list <- list.files() # list out all the DOAS files names
  
  for (file in file_list[fst:fed]){
    
    if (!exists("dataset")){      # if the merged dataset doesn't exist, create it
      dataset <- read.csv( paste(file, sep = "") )
      names(dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                          "SolarZen","Windv","Windd","RMS","HCHOaconc","HCHOfconc","HCHObconc","NO2aconc","NO2fconc","NO2bconc","SO2conc",
                          "O3conc","HCHOeconc","NO2econc","Totalextconc","avgc/molec","Totalextflux","Light")
    }else if (exists("dataset")){ # else if the merged dataset does exist, append to it
      temp_dataset <-read.csv( paste(file, sep = "") )
      names(temp_dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                               "SolarZen","Windv","Windd","RMS","HCHOaconc","HCHOfconc","HCHObconc","NO2aconc","NO2fconc","NO2bconc","SO2conc",
                               "O3conc","HCHOeconc","NO2econc","Totalextconc","avgc/molec","Totalextflux","Light")
      dataset <- rbind(dataset, temp_dataset) # bind together row-wise
      rm(temp_dataset)
    }
  }
  return(dataset)
}
