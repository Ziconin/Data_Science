readFile <- function() {
	require(data.table)

	dates <- fread("household_power_consumption.txt", na.strings="?")
	intervals <- subset(dates, Date == "1/2/2007" | Date == "2/2/2007")
	intervals	
}

plot2 <- function() {
	require(lubridate)

	data <- readFile()

	times <- dmy_hms(paste(data$Date, data$Time, sep = " "))

	png("plot2.png",width=480,height=480, bg="white")
	plot(times, data$Global_active_power, 
		type="l",
		ylab="Global Active Power (kilowatts)",
		xlab="")
	dev.off()

}