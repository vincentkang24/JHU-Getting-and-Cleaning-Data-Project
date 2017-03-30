setwd("C:\\Users\\vince\\Desktop\\Coursera\\Getting and Cleaning Data")

# Get the data
# Download and unzip the dataset from UCI webset
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "HARDataset.zip", method = "curl")
unzip(zipfile = "HARDataset.zip")

filepath <- file.path("./", "UCI HAR Dataset")
files <- list.files(filepath, recursive = TRUE)
files

# Read data from the files
# Read the Activity files
datActivityTrain <- read.table(file.path(filepath, "train", "Y_train.txt"),header = FALSE)
datActivityTest  <- read.table(file.path(filepath, "test" , "Y_test.txt" ),header = FALSE)
# Read the Subject files
datSubjectTrain <- read.table(file.path(filepath, "train", "Subject_train.txt"),header = FALSE)
datSubjectTest  <- read.table(file.path(filepath, "test" , "Subject_test.txt" ),header = FALSE)
# Read the Feature files 
datFeatureTrain <- read.table(file.path(filepath, "train", "X_train.txt"),header = FALSE)
datFeatureTest  <- read.table(file.path(filepath, "test" , "X_test.txt" ),header = FALSE)

str(datActivityTest)
str(datActivityTrain)

# 1. Merges the training and the test sets to create one data set
# (1) Concatenate the data tables by rows
datActivity <- rbind(datActivityTrain, datActivityTest)
datSubject <- rbind(datSubjectTrain, datSubjectTest)
datFeature <- rbind(datFeatureTrain, datFeatureTest)
# (2) Set names to the variables
names(datActivity) <- c("Activity")
names(datSubject) <- c("Subject")
datFeatureNames <- read.table(file.path(filepath, "features.txt"), header = FALSE)
names(datFeature) <- datFeatureNames$V2
# (3) Merge all data
Finaldata <- cbind(datActivity, datSubject, datFeature)
str(Finaldata)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# select names of features with mean() or std()
subsetFeatureNames <- datFeatureNames$V2[grep("mean\\(\\)|std\\(\\)", datFeatureNames$V2)]
# add "Activity" and "Subject" to the selectednames
selectedNames <- union(c("Activity", "Subject"), subsetFeatureNames)
# subset the final table under the mean and std condition
Finaldata2 <- subset(Finaldata, select = selectedNames)
str(Finaldata2)

# 3. Uses descriptive activity names to name the activities in the data set
# read tables from activity_labels
activityLabels <- read.table(file.path(filepath, "activity_labels.txt"),header = FALSE)
# factorize Variale activity in the data frame Data using descriptive activity names
Finaldata2$Activity <- factor(Finaldata2$Activity, labels = activityLabels$V2)
head(Finaldata2, 2)

# 4. Appropriately labels the data set with descriptive variable names.
# prefix t is replaced by time
# Acc is replaced by Accelerometer
# Gyro is replaced by Gyroscope
# prefix f is replaced by frequency
# Mag is replaced by Magnitude
# BodyBody is replaced by Body
names(Finaldata2) <- gsub("^t", "time", names(Finaldata2))
names(Finaldata2)<-gsub("^f", "frequency", names(Finaldata2))
names(Finaldata2)<-gsub("Acc", "Accelerometer", names(Finaldata2))
names(Finaldata2)<-gsub("Gyro", "Gyroscope", names(Finaldata2))
names(Finaldata2)<-gsub("Mag", "Magnitude", names(Finaldata2))
names(Finaldata2)<-gsub("BodyBody", "Body", names(Finaldata2))
# check
names(Finaldata2)

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
library(plyr)
Dataset2 <- aggregate(. ~ Subject + Activity, Finaldata2, mean)
Dataset2 <- Dataset2[order(Dataset2$Subject, Dataset2$Activity),]
write.table(Dataset2, file = "tidydata.txt",row.name=FALSE)

