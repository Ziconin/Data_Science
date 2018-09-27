constructSickness <- function(sickness) {
	temp <- NULL
	if(sickness == "heart attack") {
		temp <- 11
	}
	else if(sickness == "heart failure") {
		temp <- 17
	}
	else if(sickness == "pneumonia") {
		temp <- 23
	}
	temp
}

best <- function(state, sickness) {
	result <- character(0)
	var_sic <- constructSickness(sickness)

	if(is.null(var_sic)) {
		stop("invalid outcome")
	}
		
	outcome <- read.csv("outcome-of-care-measures.csv", 
		colClasses="character")

	if(!(state %in% outcome[, 7])) {
		stop("invalid state")
	}
	required <- outcome[, c(2,7,var_sic)]
	required <- split(required, required$State)
	
	final <- required[[state]]
	suppressWarnings(final[,3] <- as.numeric(final[,3]))

	result <- final[which(final[,3] == min(final[,3], na.rm=T)), ]
	min(as.character(result$Hospital.Name))
}