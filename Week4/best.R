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

best <- function(st,out){

  outcome_temp <- rout(out)
  outcomes_list = c("heart attack","heart failure","pneumonia")
  if(! out %in% outcomes_list) stop("invalid outcome")
  state_list <- unique(outcome_temp$State)
  if (! st %in% state_list) stop("invalid state")

  op1 <- outcome_temp[outcome_temp$State==st, ]
  op1 <- na.omit(op1)
  op1[order(op1[out],op1$Hospital.Name), ]$Hospital.Name[1]
}
best("AL","heart attack")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
