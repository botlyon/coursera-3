library(tidyverse)
library(dplyr)
getwd()
setwd("C:/temp")

#put the file in "C:/temp"
#put all data in dataframe

x1 <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y1 <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x2 <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y2 <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

#combin the training set and the test set

X <- rbind(x1, x2)
Y <- rbind(y1, y2)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

#Only extract the average and standard deviation of each measurement.

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# name the activities in the data set.

TidyData$code <- activities[TidyData$code, 2]

#labels the data set

names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#Create a second independent tidy data set containing the average of each variable for each activity and each topic.

FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))


view(FinalData)
#write the txt file
write.table(FinalData, file = "Data.txt", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
