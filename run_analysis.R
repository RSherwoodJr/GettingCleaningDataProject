#clear workspace for new project
rm(list=ls())
cat("\014") 

#set working directory
setwd("C:/Users/Randy/Dropbox/Coursera/Getting and Cleaning Data/Data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

#load required libraries
library(plyr)

#import variable list
variables <- read.table("features.txt", header=FALSE)

#since we're going to add subjectIDd to the begininng and activity at the end, insert a rows for "subjectID" and "activity"
variables$V1 <- as.numeric(as.character(variables$V1))
variables$V2 <- as.character(variables$V2)
variables <- rbind(c(1, "subjectID"), variables,c(562,"activity_code"))

#programatically create descriptive variable names
#root:
variables$Descriptive <- ifelse(grepl("BodyAcc-", variables$V2, fixed=TRUE),"Body_Acceleration",                                
                                ifelse(grepl("GravityAcc-", variables$V2, fixed=TRUE),"Gravity_Acceleration",
                                       ifelse(grepl("BodyAccJerk-", variables$V2, fixed=TRUE),"Body_Acceleration_Jerk",
                                              ifelse(grepl("BodyGyro-", variables$V2, fixed=TRUE), "Body_Gyroscope",
                                                     ifelse(grepl("BodyGyroJerk-", variables$V2, fixed=TRUE), "Body_Gyroscope_Jerk",
                                                            ifelse(grepl("BodyAccMag-", variables$V2, fixed=TRUE), "Body_Acceleration_Magnitude",
                                                                   ifelse(grepl("GravityAccMag-", variables$V2, fixed=TRUE), "Gravity_Acceleration_Magnitude",
                                                                          ifelse(grepl("BodyAccJerkMag-", variables$V2, fixed=TRUE), "Body_Acceleration_Jerk_Magnitude",
                                                                                 ifelse(grepl("BodyGyroMag-", variables$V2, fixed=TRUE), "Body_Gyroscope_Magnitude",
                                                                                        ifelse(grepl("BodyGyroJerkMag-", variables$V2, fixed=TRUE), "Body_Gyroscope_Jerk_Magnitude",
                                                                                        "NA"))))))))))
#variable type and direction (x, y, z)
variables$Descriptive2 <- paste(substr(variables$V2,1,1),"_",
                                variables$Descriptive,
                                ifelse(grepl("mean()", variables$V2, fixed=TRUE),"_Mean", 
                                       ifelse(grepl("std()", variables$V2, fixed=TRUE),"_StDev",
                                              "_NA")),
                                ifelse(grepl("X", variables$V2, fixed=TRUE), "_X", 
                                       ifelse(grepl("Y", variables$V2, fixed=TRUE), "_Y", 
                                              "_Z")),
                                sep="")

#if it's not a mean/stdev, replace with original name, to ensure uniqueness
variables$Descriptive2 <- ifelse(grepl("NA", variables$Descriptive2, fixed=TRUE), variables$V2, variables$Descriptive2)


#import activity names
activities <- read.table("activity_labels.txt", header=FALSE)
colnames(activities) <- c("activity_code", "activity")

#import test data
testSubject <- read.table("test/subject_test.txt", header=FALSE)
testX <- read.table("test/X_test.txt", header=FALSE)
testY <- read.table("test/y_test.txt", header=FALSE)

#combine test x and y
test <- cbind(testSubject, testX, testY)

#cleanup unneeded datasets
rm(testSubject)
rm(testX)
rm(testY)

#import training data
trainSubject <- read.table("train/subject_train.txt", header=FALSE)
trainX <- read.table("train/X_train.txt", header=FALSE)
trainY <- read.table("train/y_train.txt", header=FALSE)

#combine training x and y
train <- cbind(trainSubject, trainX, trainY)

#cleanup unneeded datasets
rm(trainSubject)
rm(trainX)
rm(trainY)



#stack test and training data
data1 <- rbind(test,train)

#rename data columns with descriptive names
colnames(data1) <- variables[,4]

#identify variables of interest--those containing mean and std, and the activity code and subjectID
means <- variables[grepl("mean()",variables$V2, fixed=TRUE),]
stdevs <- variables[grepl("std()",variables$V2, fixed=TRUE),]
variables.of.interest <- rbind(means,stdevs)
variables.of.interest <- variables.of.interest[,c(1,4)]
variables.of.interest <- rbind(c(1,"subjectID"),variables.of.interest,c(562,"activity_code"))

#subset the data, keeping only the variables of interest
data1 <- data1[,names(data1) %in% variables.of.interest$Descriptive2]

#assign the activity description to the data
data1 <- merge(data1, activities, by.x="activity_code", by.y="activity_code")

#drop the activity code
data1 <- data1[,!names(data1) %in% c("activity_code")]

#order the data for cleanliness
data1 <- data1[order(data1$subjectID, data1$activity),]


#calculate group averages
averages <- aggregate(data1[,2:67], list(data1$subjectID, data1$activity), mean)

#rename group names
averages <- rename(averages, c("Group.1" = "SubjectID", "Group.2" = "Activity"))

#order for cleanliness
averages <- averages[order(averages$SubjectID, averages$Activity),]


write.table(averages, "Averages.txt", row.name=FALSE)