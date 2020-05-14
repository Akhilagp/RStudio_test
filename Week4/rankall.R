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

rankall <- function(out,num="best"){
  outcome_temp<- rout(out)
  outcomes_list = c("heart attack","heart failure","pneumonia")
  if(! out %in% outcomes_list) stop("invalid outcome")

  getnum1 <- function(){
    if(num=="best")
    {
      print(st$State[1])
      num <- min(st$rank)
    } else if(num=="worst") {
      num <- max(st$rank)
    } else{
      num
    }
  }
  op_df <- data.frame()
  vect<- vector()
  op_df
  for (st in split(outcome_temp,outcome_temp$State)) {

    st <- na.omit(st)
    st$rank <- NA
    st$rank[order(st[out],st$Hospital.Name)] <- 1:nrow(st)
    st_1 <- st[st$rank==getnum1(), ]

    op_df <- rbind(op_df, data.frame(hospital=st_1$Hospital.Name[1],state =st$State[1]))
  }

  rownames(op_df) <- op_df$state
  op_df
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
rankall("heart attack",4)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

