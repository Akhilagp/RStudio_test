pollutantmean <- function(directory, pollutant, id=1:332){
  files_ = list.files(directory,full.names = T)
  df1 <- data.frame()
  for(i in files_[id]){
    df1 <- rbind(df1,read.csv(i,header = T,sep = ","))
  }
  df1$Date <- as.Date(df1$Date, format="%Y-%m-%d")
  mean(df1[[pollutant]],na.rm = T)
  }

pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata", "nitrate", 23)

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
