

rankall <- function(outcome, num = "best") {
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
	
	t3 <<- subset(data.frame(myData$State, myData$"Hospital.Name",myData[ , cind]), !is.na(myData[ , cind]))
	na.omit(t3)
	names(t3) <<- c("State", "Hospital", "MortalityRate")
	ndx <- order(t3$State, t3$MortalityRate, t3$Hospital, na.last = NA)
	t3_sorted <<- t3[ndx, ]
	names(t3_sorted) <<- c("State", "Hospital", "MortalityRate")	
	
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	split_state <- split(t3_sorted, t3_sorted$State)
	hospital_data <- lapply(split_state, function(x, num) {
				#Validate the num values
				if(num == "best") {
					rnum = 1
				}
				else if(num == "worst") {
					rnum = nrow(x)
				}
				else if(num > 0) {
					rnum = num
				}
				else {
					stop("invalid input")
					break
				}
				x$Hospital[rnum]
			}
	, num)
			
	hospital_name <- unlist(hospital_data)
	state_name <- names(hospital_data)
	data.frame(hospital = hospital_name, state = state_name, row.names = state_name)
}



