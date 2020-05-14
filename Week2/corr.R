corr <- function(directory, threshold=0){
  cor_vect <- vector("numeric")
  files_ <- list.files(directory,full.names = T)
  for (i in files_) {
    df1 <- read.csv(i,header = T, sep=",")
    comp_cases_count = nrow(df1[complete.cases(df1), ])
    if(comp_cases_count>threshold){
      cor_vect <- c(cor_vect,cor(df1$sulfate,df1$nitrate,use="pairwise.complete.obs"))
    }
  }
  print(length(cor_vect))
  cor_vect
  }
cor_vect_op <- corr("specdata",150)
head(cor_vect_op)
summary(cor_vect_op)
cor_vect_op <- corr("specdata", 400)
head(cor_vect_op)
summary(cor_vect_op)

cor_vect_op <- corr("specdata", 5000)
head(cor_vect_op)
summary(cor_vect_op)

cor_vect_op <- corr("specdata")
head(cor_vect_op)
summary(cor_vect_op)

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
help(cor)
