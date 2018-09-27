plot6 <- function() {
	require(ggplot2)
	require(dplyr)

	NEI <- readRDS("summarySCC_PM25.rds")
	##SCC <- readRDS("Source_Classification_Code.rds")

	balti <- parseData("24510", "Baltimore")
	cali <- parseData("06037", "Los Angeles")

	plottable <- rbind(balti, cali)

	qplot(year, Change, data=plottable, color=City, geom="smooth",
	main="Change in PM2.5 Emissions from Motor Vehicles", 
	xlab="Year", ylab="Change in Emissions")
	ggsave("plot6.png")
}

parseData <- function(sfips, name) {
	motors <- subset(NEI, fips == sfips & type=="ON-ROAD")

	years <- group_by(motors, year)
	emissions <- summarize(years, Emissions = sum(Emissions))

	emissions$Change <- difference(emissions$Emissions)
	emissions$City <- name
	emissions
}

difference <- function(x) {
	res <- numeric(0)
	res[1] <- 0

	for(i in 2:length(x)) {
		res[i] <- x[i] - x[i-1]
	}
	res
}