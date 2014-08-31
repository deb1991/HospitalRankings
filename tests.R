assertFail <- function(expr)
{
  tryCatch(expr(), 
           error = function(e) {
             print(sprintf("failing with expected error: '%s'", e))
           },
           finally = function() {
             stop("no, what?")
           })  
}

testBest <- function()
{
  assertFail(best(22))
  assertFail(best("zz", "puddle"))
  assertFail(best("AZ", "hiccups"))
  assertFail(best("BB", "heart attack"))
  assertFail(best("NY", "hert attack"))
  
  stopifnot(best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER")
  stopifnot(best("TX", "heart failure") =="FORT DUNCAN MEDICAL CENTER")
  stopifnot(best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE")
  stopifnot(best("MD", "pneumonia") == "GREATER BALTIMORE MEDICAL CENTER")
  
  message("Yay! Everything worked!")
}

testRankHospital <- function()
{
  assertFail(rankhospital(22))
  assertFail(rankhospital("zz", "puddle"))
  assertFail(rankhospital("AZ", "hiccups"))
  assertFail(rankhospital("BB", "heart attack"))
  assertFail(rankhospital("NY", "hert attack"))
  
  
  stopifnot(rankhospital("TX", "heart failure", 4) == "DETAR HOSPITAL NAVARRO")
  stopifnot(rankhospital("MD", "heart attack", "worst") == "HARFORD MEMORIAL HOSPITAL")
  stopifnot(is.na(rankhospital("MN", "heart attack", 5000)))
  
  stopifnot(rankhospital("TX", "heart attack", "best") == "CYPRESS FAIRBANKS MEDICAL CENTER")
  stopifnot(rankhospital("TX", "heart failure", 1) =="FORT DUNCAN MEDICAL CENTER")
  stopifnot(rankhospital("MD", "heart attack", 1) == "JOHNS HOPKINS HOSPITAL, THE")
  stopifnot(rankhospital("MD", "pneumonia", "best") == "GREATER BALTIMORE MEDICAL CENTER")
  
  message("Yay! Everything worked!")
}

testRankAll <- function()
{
  print("head(rankall(\"heart attack\", \"best\"))")
  print(head(rankall("heart attack", "best")))
  
  print("head(rankall(\"heart attack\", 20), 10)")
  print(head(rankall("heart attack", 20), 10))
  
  print("tail(rankall(\"pneumonia\", \"worst\"), 3)")
  print(tail(rankall("pneumonia", "worst"), 3))
  
  print("tail(rankall(\"heart failure\"), 10)")
  print(tail(rankall("heart failure"), 10))
}

tests <- function()
{
  testBest()
  testRankHospital()
  testRankAll()
}

tests()
