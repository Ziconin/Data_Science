readFile <- function() {
	require(data.table)

	dates <- fread("household_power_consumption.txt", na.strings="?")
	intervals <- subset(dates, Date == "1/2/2007" | Date == "2/2/2007")
	intervals	
}

plot3 <- function() {
	require(lubridate)

	data <- readFile()
	times <- dmy_hms(paste(data$Date, data$Time, sep = " "))

	png("plot3.png",width=480,height=480, bg="white")
	plot(times, data$Sub_metering_1, 
		type="n",
		ylab="Energy sub metering",
		xlab=""
	)
	lines(times, data$Sub_metering_1, 
		col="black"
	)
	lines(times, data$Sub_metering_2,
		col="red"
	)
	lines(times, data$Sub_metering_3,
		col="blue"
	)

	legend("topright",
		lty = c(1,1,1), 
		col=c("black","red","blue"), 
		legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		cex=0.85
	)
	dev.off()

}