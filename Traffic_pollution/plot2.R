plot2 <- function() {
	require(dplyr)

	NEI <- readRDS("summarySCC_PM25.rds")
	##SCC <- readRDS("Source_Classification_Code.rds")

	balti <- subset(NEI, fips == "24510")
	years <- group_by(balti, year)
	plottable <- with(years, summarize(years, Emissions = sum(Emissions)))

	png("plot2.png",width=480,height=480, bg="white")
	with(plottable, plot(year, Emissions, 
					type="l", 
					main = "Emissions in Baltimore City, Maryland, 1999-2008",
					xlab="Year",
					ylab="Total PM2.5 emissions"
				))
	dev.off()
}