rankall <- function(outcome, num = "best")
{
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

  # convert target metric to numeric, first translating and then filterin for "N/A" values
  best.hospital.data[best.hospital.data[,column.name]=="Not Available",column.name] <- NA
  best.hospital.data[,column.name] <- as.numeric(best.hospital.data[,column.name])
  best.hospital.data <- best.hospital.data[!is.na(best.hospital.data[,column.name]),]
  
  # Create an abbreviated data set to avoid hugeness in what follows    
  hospitals <- best.hospital.data[c("State", "Hospital.Name", column.name)]
  
  #split by state
  hospitalsByState <- split(hospitals, factor(hospitals$State))
  
  # Grab the set of states - we'll need that in a bit
  state <- levels(factor(hospitals$State))
  #print(state)
  #print(hospitals)
  hospital <- NULL
  for (s in state)
  {
    #sp[[s]][order(sp[[s]][,3]),]
    x = hospitalsByState[[s]]
    sorted <- x[order(x[,3],x[,2]),]
    row = num
    if (row == "best")
    {
      row = 1
    }
    else if (num == "worst")
    {
      row = nrow(sorted)
    }
    
    hospital[s] <- sorted[row,2]
  }
  
  data.frame(hospital,state)
}
