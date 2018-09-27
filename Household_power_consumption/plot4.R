readFile <- function() {
	require(data.table)

	dates <- fread("household_power_consumption.txt", na.strings="?")
	intervals <- subset(dates, Date == "1/2/2007" | Date == "2/2/2007")
	intervals	
}

plot4 <- function() {
	require(lubridate)

	data <- readFile()
	times <- dmy_hms(paste(data$Date, data$Time, sep = " "))
	
	png("plot4.png",width=480,height=480, bg="white")

	par(mfcol = c(2,2))

	##plot 1

	plot(times, data$Global_active_power, 
		type="l",
		ylab="Global Active Power",
		xlab="",
		cex.lab = 0.8,
		cex.axis = 0.8
	)

	##plot 2
	
	plot(times, data$Sub_metering_1, 
		type="n",
		ylab="Energy sub metering",
		xlab="", 
		cex.lab = 0.8,
		cex.axis = 0.8
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
		cex=0.85,
		bty="n"
	)

	## plot 3

	plot(times, data$Voltage,
		type="l",
		col="black",
		ylab="Voltage",
		xlab="datetime",
		cex.lab = 0.8,
		cex.axis = 0.8
	)

	## plot 4
	plot(times, data$Global_reactive_power,
		type="l",
		col="black",
		ylab="Global_reactive_power",
		xlab="datetime",
		cex.lab = 0.8,
		cex.axis = 0.8
	)
	dev.off()

}