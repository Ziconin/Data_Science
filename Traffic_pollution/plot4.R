plot4 <- function() {
	require(dplyr)
	require(ggplot2)

	NEI <- readRDS("summarySCC_PM25.rds")
	SCC <- readRDS("Source_Classification_Code.rds")

	coalcomb <- SCC[grepl("Coal", SCC$Short.Name, fixed=TRUE), ]

	data <- merge(coalcomb, NEI, by="SCC")
	years <- group_by(data, year)
	plottable <- summarize(years, Emissions = sum(Emissions))

	qplot(year, Emissions, data=plottable, geom="smooth",
		main="PM2.5 Emissions from Coal Combustion-related sources, 1999-2008",
		xlab="Year", ylab="Total PM2.5 emissions" 
	)
	ggsave("plot4.png")	
}

	
	
	