readFile <- function() {
	require(data.table)

	dates <- fread("household_power_consumption.txt", na.strings="?")
	intervals <- subset(dates, Date == "1/2/2007" | Date == "2/2/2007")
	intervals	
}

plot1 <- function() {
	data <- readFile()
	par(mar = c(7,5,1,1))

	png("plot1.png",width=480,height=480, bg="white")
	hist(data$Global_active_power, 
		main="Global Active Power", col="red", 
		xlab="Global Active Power (kilowatts)")
	dev.off()
}