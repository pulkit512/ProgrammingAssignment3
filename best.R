

best <- function(state, outcome) {
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
	
	if(!(state %in% myData$State)) {
		stop("Invalid State")
		break
	}
	
	## Return hospital name in that state with lowest 30-day death
	## rate	
	t1 <<- subset(data.frame(myData$State, myData$"Hospital.Name",myData[ , cind]),myData$State == state & !is.na(myData[ , cind]))
	na.omit(t1)
	names(t1) <<- c("State", "Hospital", "MortalityRate")	
	#t1$MortalityRate <<- as.numeric(as.character(t1$MortalityRate))
	t1$Hospital[which.min(t1$MortalityRate)]
}


