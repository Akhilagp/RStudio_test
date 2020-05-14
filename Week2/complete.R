complete <- function(directory,id=1:332){
  files_ = list.files(directory,full.names = T)
  df_op <- data.frame()
  #print(names(df_op))
  for(i in files_[id]){
    df1 <- read.csv(i,header = T,sep = ",")
    id <- unique(df1$ID)
    nobs <- nrow(df1[complete.cases(df1), ])
    df_op <- rbind(df_op,c(id,nobs))
  }
  names(df_op)<- c("id","nobs")
  df_op
}
complete("specdata",1)
complete("specdata", c(2, 4, 8, 10, 12))  
complete("specdata", 30:25)
complete("specdata", 3)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

