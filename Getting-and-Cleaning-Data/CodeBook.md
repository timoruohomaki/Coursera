## Getting and Cleanind Data Course Project

The purpose of this project is to demonstrate your ability to collect,
work with, and clean a data set. The goal is to prepare tidy data that
can be used for later analysis.

The assignment deliverable is a R script that does the following:

1.  Merges the training and the test sets to create one data set.

2.  Extracts only the measurements on the mean and standard deviation
    for each measurement.

3.  Uses descriptive activity names to name the activities in the data
    set

4.  Appropriately labels the data set with descriptive variable names.

5.  From the data set in step 4, creates a second, independent tidy data
    set with the average of each variable for each activity and each
    subject.

#### Dependencies (Packages)

``` r
library(readr)
library(knitr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

#### Source Code

``` r
  # 1) creating all the data frames, one for each type of observations

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
  
  summary(subjectData)
```

    ##     subject     
    ##  Min.   : 1.00  
    ##  1st Qu.: 9.00  
    ##  Median :17.00  
    ##  Mean   :16.15  
    ##  3rd Qu.:24.00  
    ##  Max.   :30.00

#### Step 3:Create one master (merged) dataset

``` r
  # 1) create one master (merged) dataset

  masterDataset <- cbind(subjectData, xData, yData)

# 2) extract only mean and std dev for each measurement

  extractDataset <- masterDataset %>% select(subject, code, contains("mean"), contains("std"))

  extractDataset$code <- activityLabels[extractDataset$code, 2]

  summary(extractDataset$code)
```

    ##    Length     Class      Mode 
    ##     10299 character character
