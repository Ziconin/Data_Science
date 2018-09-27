plot5 <- function() {
	require(ggplot2)
	require(dplyr)

	NEI <- readRDS("summarySCC_PM25.rds")
	##SCC <- readRDS("Source_Classification_Code.rds")

	baltimotor <- subset(NEI, fips == "24510" & type=="ON-ROAD")
	years <- group_by(baltimotor, year)
	plottable <- summarize(years, Emissions=sum(Emissions))

	qplot(year, Emissions, data=plottable, geom="smooth",
		main="PM2.5 Emissions from Motor Vehicles in Baltimore City, 1999-2008",
		xlab="Year", ylab="PM2.5 Emissions")
	ggsave("plot5.png")
}