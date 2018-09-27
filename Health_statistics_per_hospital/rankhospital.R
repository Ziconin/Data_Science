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

constructIndex <- function(i, len) {
	index <- NULL
	if(i == "best") {
		index <- 1
	}
	else if(i == "worst") {
		index <- len
	}
	else {
		index <- as.numeric(i)
	}
	index
}

rankhospital <- function(state, sickness, num = "best") {
	result <- character(0)
	
	if(is.null(var_sic <- constructSickness(sickness))) {
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

	ranked <- data.frame("Hospital.Name"=final[,1], "Rate"=final[,3])
	ranked <- ranked[order(ranked$Rate, ranked$Hospital.Name,
		na.last=NA),]
	ranked[, "Rank"] <- 1:nrow(ranked)

	index <- constructIndex(num, nrow(ranked))
	result <- as.character(ranked[index,]$Hospital.Name)

	result
}