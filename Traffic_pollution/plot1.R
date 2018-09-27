plot1 <- function() {
	require(dplyr)

	NEI <- readRDS("summarySCC_PM25.rds")
	##SCC <- readRDS("Source_Classification_Code.rds")
	
	years <- group_by(NEI, year)
	plottable <- with(years, summarize(years, Emissions = sum(Emissions)))

	png("plot1.png",width=480,height=480, bg="white")
	with(plottable, plot(year, Emissions, 
					type="l", 
					main = "Emissions in the U.S. 1999-2008",
					xlab="Year",
					ylab="Total PM2.5 emissions"
					))
	dev.off()	
}