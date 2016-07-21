


rankhospital <- function(state, outcome, num = "best") {
	## Read csv data file
	myData <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
	
	## Read outcome data
	## Check that state and outcome are valid
	if(outcome == "heart attack") {
		cind <- 11
	}
	else if(outcome == "heart failure") {
		cind <- 17
	}
	else if(outcome == "pneumonia") {
		cind <- 23
	}
	else {
		stop("Invalid Outcome")
		break
	}
	
	# Validate the state
	if(!(state %in% myData$State)) {
		stop("Invalid State")
		break
	}
	
	
	t2 <<- subset(data.frame(myData$State, myData$"Hospital.Name",myData[ , cind]),myData$State == state & !is.na(myData[ , cind]))
	na.omit(t2)
	
	#Validate the num values
	if(num == "best") {
		rnum = 1
	}
	else if(num == "worst") {
		rnum = nrow(t2)
	}
	else if(num > 0) {
		rnum = num
	}
	else {
		stop("invalid input")
		break
	}
		
	## Return hospital name in that state with the given rank
	## 30-day death rate	
	names(t2) <<- c("State", "Hospital", "MortalityRate")
	t2$Ranking <<- rank(t2$MortalityRate, ties.method= "last")
	t2$Hospital[t2$Ranking == rnum]
}


