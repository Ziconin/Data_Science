## Download and unzip the files, if they do not exist yet. 
## Else, skip this step.

getFiles <- function(dest) {
	if(!file.exists(dest)) {
		download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
			destfile=dest)
		unzip(dest)
	} else if(!file.exists("UCI HAR Dataset")) {
		unzip(dest)
	}
	else {}
}

## Parse filepaths for each file necessary.
## Return full paths for further use.

getFilePaths <- function() {
	filenames <- character()
         
	filenames[c(1,2)] <- list.files("UCI HAR Dataset")[c(1,2)]

	trains <- list.files("UCI HAR Dataset/train", pattern="\\.txt$")
	filenames[3:5] <- paste("train", trains, sep="/")

	tests <- list.files("UCI HAR Dataset/test", pattern="\\.txt$")
	filenames[6:8] <- paste("test", tests, sep="/")
	
	filenames <- paste("UCI HAR Dataset", filenames, sep="/")
	names(filenames) = c("act", "feat", "tr_sub", "tr_x", "tr_y",
		"ts_sub", "ts_x", "ts_y")
	filenames	
}

## Helper function to allow different binds with the same call.

combine <- function(..., FUN) {
	result <- FUN(...)
	result
}

## Loading the bulk of data from files.
## First, combining train-data and test-data to their own tables,
## then combine the rows as the whole set.
## Take relevant rows from features.txt and parse them into column names.
## Set column names for the complete dataset.
## Factorize the subject- and activity-column and switch numbers to strings 
## from activity_labels.txt.

loadData <- function(files) {
	activities <- read.table(files["act"])
	features <- read.table(files["feat"])

	feature_vals <- grep("mean|std", features[,2])

	train_set <- read.table(files["tr_x"])[feature_vals]	
	train_labels <- read.table(files["tr_y"])
	train_subs <- read.table(files["tr_sub"])
	train_all <- combine(train_subs, train_labels, train_set, FUN=cbind)

	test_set <- read.table(files["ts_x"])[feature_vals]
	test_labels <- read.table(files["ts_y"])
	test_subs <- read.table(files["ts_sub"])
	test_all <- combine(test_subs, test_labels, test_set, FUN=cbind)

	all <- combine(train_all, test_all, FUN=rbind)

	feature_names <- features[feature_vals, 2]
	feature_names <- gsub("[-|(|)]","",feature_names)	
	feature_names <- gsub("mean","Mean",feature_names)
	feature_names <- gsub("std","Std",feature_names)

	names(all) = c("subject", "activity", feature_names)

	all$subject <- as.factor(all$subject)
	all$activity <- factor(all$activity, 
		levels=activities[,1], labels=as.character(activities[,2]))

	all
}

## Clean and parse the data into a tidy form with reshape2-library.
## Melt the data, using subject-activity pair as primary key.
## Take means from the melted set by calculating variable means for every
## subject-activity pair. 
## Finally save the data into a txt-file.

finalize <- function(all) {
	require(reshape2)

	melted <- melt(all, id=c("subject", "activity"))
	casted <- dcast(melted, subject + activity ~ variable, mean)

	write.table(casted, "activitymeans.txt", 
		quote=FALSE, row.names=FALSE)
}

## High-level function to maintain the proper algorith for cleaning.

main <- function() {
	getFiles("project.zip")
	allData <- loadData(getFilePaths())
	finalize(allData)
}

