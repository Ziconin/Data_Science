#Getting and Cleaning Data Course Project

The program contain six functions (also documented in run_analysis.R):
* getFiles()
* getFilePaths() 
* combine()
* loadData() 
* finalize()
* main() 

####getFiles()
Download and unzip the files, if they do not exist yet. Else, skip this step.
####getFilePaths()
Parse filepaths for each file necessary. Return full paths for further use.
####combine()
Helper function to allow different binds with the same call.
####loadData()
Loading the bulk of data from files. 
First, combining train-data and test-data to their own tables, then combine the rows as the whole set.
Take relevant rows from features.txt and parse them into column names. Set column names for the complete dataset.
Factorize the subject- and activity-columns and switch numbers to strings from activity_labels.txt.
####finalize()
Clean and parse the data into a tidy form with reshape2-library.
Melt the data, using subject-activity pair as primary key.
Take means from the melted set by calculating variable means for every subject-activity pair. 
Finally save the data into a txt-file (activitymeans.txt).
####main()
High-level function to maintain the proper algorith for cleaning.
