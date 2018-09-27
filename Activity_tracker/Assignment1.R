require(dplyr)
require(ggplot2)
require(timeDate)

main <- function() {
	activity <- read.csv("activity.csv")
	
	clean_act <- activity[complete.cases(activity), ]

################################

	total_steps <- aggregate(clean_act[, 1], list(clean_act$date), sum)
	colnames(total_steps) <- c("Date", "Total")

	qplot(total_steps$Total, 
		geom="histogram", 
		binwidth=5000,
		main="Histogram of total number of steps taken per day",
		xlab="Total number of steps in a day",
		ylab="Frequency",
		fill=I("white"),
		col=I("black"))

	summary(total_steps)

################################

	total_intervals <- aggregate(clean_act[, 1], 
		list(clean_act$interval), mean)
	colnames(total_intervals) <- c("Interval", "Average")

	qplot(total_intervals$Interval, 
		total_intervals$Average, 
		geom="line",
		main="Average number of steps taken over all days",
		xlab="Intervals",
		ylab="Average number of steps")

	total_intervals[total_intervals$Average == max(total_intervals$Average), ]

################################

	sum(is.na(activity))

	new_act <- activity
	for(i in 1:nrow(new_act)) {
		if(is.na(new_act$steps[i])) {
			interval_value <- new_act$interval[i]
			step_value <- total_intervals[
				total_intervals$Interval == interval_value, ]
			new_act$steps[i] <- step_value$Average
		} 
	}

	new_steps <- aggregate(new_act[, 1], list(new_act$date), sum)
	colnames(new_steps) <- c("Date", "Total")

	qplot(new_steps$Total, 
		geom="histogram", 
		binwidth=5000,
		main="Histogram of total number of steps taken per day (imputed)",
		xlab="Total number of steps in a day",
		ylab="Frequency",
		fill=I("white"),
		col=I("black"))

	summary(new_steps)

################################

	new_act$weekdays <- factor(isWeekday(as.Date(new_act$date)), 
		levels=c(TRUE, FALSE), labels=c("weekday", "weekend"))	

	new_intervals <- aggregate(steps ~ interval + weekdays, new_act, mean)
	colnames(new_intervals) <- c("Interval", "DayType", "Average")
	
	p <- ggplot(new_intervals, aes(Interval, Average)) + geom_line()
	p + facet_grid(DayType ~ .) + labs(y="Average number of steps")
}

################################


