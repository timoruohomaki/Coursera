# COURSERA Getting and Cleaning Data - Week 4 Assignment

# 1) Merges the training and the test sets to create one data set.

# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 

# 3) Uses descriptive activity names to name the activities in the data set

# 4) Appropriately labels the data set with descriptive variable names. 

# 5) From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

# NOTE: Dataset downloaded and unzipped offline on ~/Dev/R/Coursera/GettingAndCleaningData/Week4/ucidataset

library(dplyr)
library(readr)
# libraries for notebook generation:
library(knitr)

# getting and unzipping the dataset

if(!file.exists("./data")){dir.create("./ucidataset")}



# creating all the data frames, one for each type of observations

featureData <- read.table("ucidataset/features.txt", col.names = c("n","functions"))
activityLabels <- read.table("ucidataset/activity_labels.txt", col.names = c("code","activity"))
subjectTest <- read.table("ucidataset/test/subject_test.txt", col.names = "subject")

xTestData <- read.table("ucidataset/test/X_test.txt", col.names = featureData$functions)
yTestData <- read.table("ucidataset/test/Y_test.txt", col.names = "code")

subjectTrain <- read.table("ucidataset/train/subject_train.txt", col.names = "subject")

xTrainData <- read.table("ucidataset/train/X_train.txt", col.names = featureData$functions)
yTrainData <- read.table("ucidataset/train/Y_train.txt", col.names = "code")

# 1) merge test and training data

xData <- rbind(xTrainData, xTestData)
yData <- rbind(yTrainData, yTestData)

subjectData <- rbind(subjectTrain, subjectTest)

# 1) create one master (merged) dataset

masterDataset <- cbind(subjectData, xData, yData)

# 2) extract only mean and std dev for each measurement

extractDataset <- masterDataset %>% select(subject, code, contains("mean"), contains("std"))

# 3) apply descriptive activity names

extractDataset$code <- activityLabels[extractDataset$code, 2]

# testing: head(extractDataset)

# 4) apply appropriate labels with descriptive variable names

names(extractDataset)[1] = "Subject"
names(extractDataset)[2] = "Activity"
names(extractDataset) <- gsub("Acc", "Accelerometer", names(extractDataset))
names(extractDataset) <- gsub("BodyBody", "Body", names(extractDataset))
names(extractDataset) <- gsub("Gyro", "Gyroscope", names(extractDataset))
names(extractDataset) <- gsub("Mag", "Magnitude", names(extractDataset))
names(extractDataset) <- gsub("^t", "Time", names(extractDataset))
names(extractDataset) <- gsub("^f", "Frequency", names(extractDataset))
names(extractDataset) <- gsub("tBody", "TimeBody", names(extractDataset))
names(extractDataset) <- gsub("-mean()", "Mean", names(extractDataset), ignore.case = TRUE)
names(extractDataset) <- gsub("-std()", "STD", names(extractDataset), ignore.case = TRUE)
names(extractDataset) <- gsub("-freq()", "Frequency", names(extractDataset), ignore.case = TRUE)
names(extractDataset) <- gsub("angle", "Angle", names(extractDataset))
names(extractDataset) <- gsub("gravity", "Gravity", names(extractDataset))

# 5) Create second, independent tidy data set with averages and export it as a file

dataProduct <- extractDataset %>%
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))

write.table(dataProduct, "UCI-DataProduct.txt", row.name=FALSE)
write_excel_csv(dataProduct, "UCI-DataProduct.csv")



