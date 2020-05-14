rout <- function(out){
  outcome_temp <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  names(outcome_temp)[11] <- "heart attack"
  names(outcome_temp)[17] <- "heart failure"
  names(outcome_temp)[23] <- "pneumonia"
  outcome_temp <- outcome_temp[ , c("Hospital.Name","State",out)]
  outcome_temp[outcome_temp=="Not Available"] <- NA
  outcome_temp[, out] <- as.numeric(outcome_temp[, out])

  outcome_temp
}

rankhospital <- function(st,out,num="best"){
  outcome_temp <- rout(out)
  outcomes_list = c("heart attack","heart failure","pneumonia")
  if(! out %in% outcomes_list) stop("invalid outcome")

  state_list <- unique(outcome_temp$State)
  if (! st %in% state_list) stop("invalid state")

  op1 <- outcome_temp[outcome_temp$State==st, ]

  getnum <- function(){
    if(num=="best")
    {
      num <- min(op1$rank)
    } else if(num=="worst") {
      num <- max(op1$rank)
    } else {
      num
    }

  }
  op1 <- na.omit(op1)
  op1$rank <- NA
  op1$rank[order(op1[out],op1$Hospital.Name)] <- 1:nrow(op1)

  op1[op1$rank==getnum(), ]$Hospital.Name[1]

}
rankhospital("TX","heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
rankhospital("AK","heart attack",20)
rankhospital("WI", "pneumonia", "worst")

