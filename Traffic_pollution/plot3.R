plot3 <- function() {
	require(ggplot2)
	require(dplyr)

	NEI <- readRDS("summarySCC_PM25.rds")
	##SCC <- readRDS("Source_Classification_Code.rds")

	balti <- subset(NEI, fips == "24510")
	types <- group_by(balti, type, year)
	grouped <- summarize(types, Emissions = sum(Emissions))

	qplot(year, Emissions, data=grouped, color=type, geom="smooth", 
		main="Emissions in Baltimore City, Maryland, 1999-2008",
		xlab="Year", ylab="Total PM2.5 emissions")	
	ggsave("plot3.png")
}