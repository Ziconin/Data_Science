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

constructIndex <- function(l, i) {
	index <- NULL
	if(i == "best") {
		index <- 1
	}
	else if(i == "worst") {
		index <- nrow(l)
	}
	else {
		index <- as.numeric(i)
	}
	index
}

sortall <- function(l) {
	suppressWarnings(l[,3] <- as.numeric(l[,3]))
	l <- l[order(l[,3], l[,1], na.last=NA),]
	l[,4] <- 1:nrow(l)
	l
}

assign <- function(l, num) {
	index <- constructIndex(l, num)
	l[which(l[,4] == index), ]
}

finalize <- function(res, num) {
	len <- length(res)
	
	x <- character(len)
	y <- character(len)

	states <- sapply(res, function(l) {l["State"]})
	h_names <- lapply(res, assign, num)

	for(i in 1:len) {
		y[i] <- states[[c(i,1)]]

		temp1 <- c(h_names[[c(i,1)]]) 
		if(length(temp1) != 1) {
			temp1 <- NA
		}
		x[i] <- temp1	
	}
	
	temp <- data.frame(hospital=x,state=y, stringsAsFactors=F)
	row.names(temp) <- y
	temp
	
}

rankall <- function(sickness, num = "best") {
	
	if(is.null(var_sic <- constructSickness(sickness))) {
		stop("invalid outcome")
	}
		
	outcome <- read.csv("outcome-of-care-measures.csv", 
		colClasses="character")

	required <- outcome[, c(2,7,var_sic)]
	required <- split(required, required$State)
	
	ranked <- lapply(required, sortall)

	result <- finalize(ranked, num)
	result

	
	
}