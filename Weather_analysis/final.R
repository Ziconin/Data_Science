require(stringdist)
require(ggplot2)

main <- function() {
	if(!file.exists("StormData.csv.bz2")) {
		download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")
	}
	read.csv("StormData.csv.bz2", stringsAsFactors=FALSE)
}

healthdmg <- function(data) {
	injuries <- aggregate(INJURIES ~ EVTYPE, data, sum)
	fatalities <- aggregate(FATALITIES ~ EVTYPE, data, sum)

	inj <- injuries[injuries$INJURIES > 0, ]
	fats <- fatalities[fatalities$FATALITIES > 0, ]
	
	temp <- process_ev(inj)
	injured <- head(temp[order(temp$SUM, decreasing=T),], 10)

	temp <- process_ev(fats)
	fatal <- head(temp[order(temp$SUM, decreasing=T),], 10)

	ggplot(injured, aes(x=reorder(EVTYPE, SUM), y=SUM)) +
		geom_bar(stat="identity") +
		coord_flip() +
		xlab("Event type") + ylab("Total number of injuries")
	
	ggplot(fatal, aes(x=reorder(EVTYPE, SUM), y=SUM)) + 
		geom_bar(stat="identity") + 
		coord_flip() + 
		xlab("Event type") + ylab("Total number of fatalities")	
}

econdmg <- function(data) {
	propdmg <- aggregate(PROPDMG ~ EVTYPE + PROPDMGEXP, data, sum)
	propdmg[, c(2,3)] <- process_mag(propdmg[, c(2,3)])	
	
	temp <- process_ev(propdmg[, c(1,3)])
	properties <- head(temp[order(temp$SUM, decreasing=T),], 10)

	cropdmg <- aggregate(CROPDMG ~ EVTYPE + CROPDMGEXP, data, sum)
	cropdmg[, c(2,3)] <- process_mag(cropdmg[, c(2,3)])

	temp <- process_ev(cropdmg[, c(1,3)])
	crops <- head(temp[order(temp$SUM, decreasing=T),], 10)

	temp <- rbind(properties, crops)
	dmgs <- aggregate(SUM ~ EVTYPE, temp, sum)
	dmgs$SUM <- dmgs$SUM / 1000000000

	ggplot(dmgs, aes(x=reorder(EVTYPE, SUM), y=SUM)) +
		geom_bar(stat="identity") +
		coord_flip() +
		xlab("Event type") + 
		ylab("Total cost of damage to crops and properties (in billions)") +
		geom_text(aes(label=round(SUM)))
}

process_mag <- function(data) {
	new_data <- data.frame(DMG=numeric())
	for(i in 1:nrow(data)) {
		row <- data[i, ]
		mag <- row[, 1]

		if(mag == "K" | mag == "k") {
			new_data[i, 2] <- row[, 2] * 1000
		}
		else if(mag == "M" | mag == "m") {
			new_data[i, 2] <- row[, 2] * 1000000
		}	
		else if(mag == "B" | mag == "b") {
			new_data[i, 2] <- row[, 2] * 1000000000
		}
	}
	new_data
}

process_ev <- function(data) {
	names <- c("Astronomical Low Tide","Avalanche","Blizzard",
		"Coastal Flood","Cold/Wind Chill","Debris Flow",
		"Dense Fog","Dense Smoke","Drought","Dust Devil",
		"Dust Storm","Excessive Heat","Extreme Cold/Wind Chill",
		"Flash Flood","Flood","Frost/Freeze","Funnel Cloud",
		"Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow",
		"High Surf","High Wind","Hurricane (Typhoon)","Ice Storm",
		"Lake-Effect Snow","Lakeshore Flood","Lightning",
		"Marine Hail","Marine High Wind","Marine Strong Wind",
		"Marine Thunderstorm Wind","Rip Current","Seiche","Sleet",
		"Storm Surge/Tide","Strong Wind","Thunderstorm Wind","Tornado",
		"Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash",
		"Waterspout","Wildfire","Winter Storm","Winter Weather")

	names <- toupper(names)
	tempnames <- toupper(data$EVTYPE)

	colnames(data) <- c("EVTYPE", "SUM")

	for(i in 1:length(tempnames)) {
		row <- tempnames[i]

		index_names <- agrep(row, names, max.distance = 2, ignore.case=TRUE)
		index_list <- agrep(row, tempnames, max.distance = 2, ignore.case=TRUE)

		if(length(index_names) == 1 & length(index_list) == 1) {
			tempnames[index_list] <- names[index_names]
		}
		else if(length(index_names) == 0 & length(index_list) == 1) {}
		else if(length(index_names) == 2) {
			temp <- names[index_names]

			type <- adist(row, temp, ignore.case=TRUE)
			if(min(type) < 9) {
				index <- which(min(type) == type)
				tempnames[i] <- temp[index]
			}
		}
		else {
			temp <- names[amatch(row, names, maxDist=2, method="lv")]
			if(!is.na(temp)) {tempnames[i] <- temp} 
		}

	}
	data$EVTYPE <- tempnames
	aggregate(SUM ~ EVTYPE, data, sum)
}