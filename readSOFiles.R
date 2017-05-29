
readSOFiles <- function(fst,fed,dateIn,gas) {
  
  # Read a bunch of SOF files recursively and merge them together in row-wise.
  # Whether to read Butane05_d05_xxxx.csv file or C3H6_d05_xxxx_0.csv file only depends on different fst and fed index chosen.
  # 2017-03-07-08, 1st built, Weihua Wang
  # fst: the first csv file index in the listed out file_list
  # fed: the last csv file index in the listed out file_list
  # dateIn: the specific date, e.g. 160508
  
  setwd( paste("/home/wien/Octave/flameDOAS/GPS_SOF/", toString(dateIn), "/", sep="") )
  
  file_list <- list.files()
  
  if (gas=="NH3") {
    for (file in file_list[fst:fed]){
      if (!exists("dataset")){                  # if the merged dataset doesn't exist, create it
        dataset <- read.csv( paste(file, sep = "") )
        names(dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                            "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4conc","NH3conc","C3H6conc","Totalextconc","avgc/molec",
                            "Totalextflux","Light")
        dataset <- dataset[-1, 1:24]            # remove the first row and keep the rest
      } else if (exists("dataset")){            # if the merged dataset does exist, append to it
        temp_dataset <-read.csv( paste(file, sep = "") )
        names(temp_dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                                 "SolarZen","Windv","Windd","RMS","IgramNoise","C2H4conc","NH3conc","C3H6conc","Totalextconc","avgc/molec",
                                 "Totalextflux","Light")
        temp_dataset <- temp_dataset[-1, 1:24]  # remove the first row and keep the rest
        dataset <- rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }
  } else if (gas=="C4H10") {
    for (file in file_list[fst:fed]){
      if (!exists("dataset")){                  # if the merged dataset doesn't exist, create it
        dataset <- read.csv( paste(file, sep = "") )
        names(dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                            "SolarZen","Windv","Windd","RMS","IgramNoise","C3H8conc","C4H10conc","C8H18conc","Totalextconc","avgc/molec",
                            "Totalextflux","Light")
        dataset <- dataset[-1, 1:24]            # remove the first row and keep the rest
      } else if (exists("dataset")){            # if the merged dataset does exist, append to it
        temp_dataset <-read.csv( paste(file, sep = "") )
        names(temp_dataset) <- c("FileName","Daytime","TimeFromStart","DistFromStart","dx","Xpos","Ypos","Lat","Lon","Ftot","F3","SolarAzim",
                                 "SolarZen","Windv","Windd","RMS","IgramNoise","C3H8conc","C4H10conc","C8H18conc","Totalextconc","avgc/molec",
                                 "Totalextflux","Light")
        temp_dataset <- temp_dataset[-1, 1:24]  # remove the first row and keep the rest
        dataset <- rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
    }
  }

  return(dataset)
}
