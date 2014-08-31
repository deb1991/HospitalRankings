rankhospital <- function(state, outcome, num = "best") { ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank ## 30-day death rate
  if (is.null(state) || !is.character(state))
  {
    stop("invalid state")
  }
  
  column.names = cbind(
    c("heart attack", 
      "heart failure", 
      "pneumonia"),
    c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    ))
  
  possible.outcomes = factor()
  if (!(outcome %in% column.names[,1]))
  {
    stop("invalid outcome")
  }
  
  column.name = column.names[column.names[,1] == outcome,2]
  #print(sprintf("translating '%s' to %s", outcome, column.name))
  
  # read the data from the CSV file
  best.hospital.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #message("data loaded and parameters validated")
  
  # validate the state
  if (sum(best.hospital.data$State == state) == 0)
  {
    stop("invalid state")
  }
  
  #filter for just hospitals in the target state
  hospitals.in.state <- best.hospital.data[best.hospital.data$State == state,]
  #print(sprintf("found %d hospitals in %s", nrow(hospitals.in.state), state))
  
  #convert target metric to numeric
  hospitals.in.state[hospitals.in.state[,column.name]=="Not Available",column.name] <- NA
  hospitals.in.state[,column.name] <- as.numeric(hospitals.in.state[,column.name])
  #print(hospitals.in.state[,column.name])
  
  hospitals <- hospitals.in.state[c("Hospital.Name", column.name)]
  #print(hospitals)
  
  hospitals <- hospitals[!is.na(hospitals[2]),]
  #print(hospitals)
  
  hospitals <- hospitals[order(hospitals[2],hospitals[1]),]
  #print(hospitals)
  
  if (num == "best")
  {
    num = 1
  }
  else if (num == "worst")
  {
    num = nrow(hospitals)
  }

  #print(hospitals)
  hospitals[num,1]
}
