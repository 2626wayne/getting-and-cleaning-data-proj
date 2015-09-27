##########################################################################################################

## Coursera Getting and Cleaning Data Course Project

##########################################################################################################

# 1. Merge the training and the test sets to create one data set.

#set the data filepath
setwd("./Course Project/")
#reading in core data
features <- read.table("./features.txt", header=F)
activityType <- read.table("./activity_labels.txt", header = F)
#reading in test data
subjectTest <- read.table("./test/subject_test.txt", header = F)
xTest <- read.table("./test/X_test.txt", header = F)
yTest <- read.table("./test/y_test.txt", header = F)
#reading in train data
subjectTrain <- read.table("./train/subject_train.txt", header = F)
xTrain <- read.table("./train/X_train.txt", header = F)
yTrain <- read.table("./train/y_train.txt", header = F)

#explore data
head(features, n=5)
head(activityLabels, n=5)
head(subjectTest, n=5)
head(xTest, n=5)
head(xTrain, n=5)
head(subjectTrain, n=5)
head(xTest, n=5)
head(xTrain, n=5)
summary(features)
summary(features)
summary(activityLabels)
summary(subjectTest)
summary(xTest)
summary(xTrain)
summary(subjectTrain)
summary(xTest)
summary(xTrain)


colnames(activityType) <- c("activityId", "activityType") #adds column names to activityLabels

#Assigning column names to train data
colnames(xTrain) <- features[,2] #adds the second column of features as the column name to XTrain
colnames(yTrain) <- c("activityId")
colnames(subjectTrain) <- c("subjectId")
trainingData <- cbind(yTrain, subjectTrain, xTrain)

#Assigning column names to test data
colnames(xTest) <- features[,2] #adds the second column of features as the column name to XTest
colnames(yTest) <- c("activityId")
colnames(subjectTest) <- c("subjectId")
testData <- cbind(yTest, subjectTest, xTest)

#Combine the two data sets

finalData <- rbind(testData, trainingData)

##########################################################################################################

#2. Extract only the measurements on the mean and standard deviation for each measurement. 

colNames <- colnames(finalData) #creates a vector of column names from finalData
#create a logical vector using GREPL to extract only the columns with mean and standard deviation.
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
head(logicalVector) #first two vectors should be true
finalData <- finalData[logicalVector==TRUE] #identifys only the columns where mean and std = true

##########################################################################################################

#3. Use descriptive activity names to name the activities in the data set

finalData <- merge(finalData, activityType, by="activityId", all.x=TRUE)
colNames  <- colnames(finalData)


# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colNames -> colnames(finalData) 

##########################################################################################################

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

finalData2 <- finalData[,names(finalData) != 'activityType']; # New table with no activityType

#summarize only the mean of each variable by activity and subject
tidyData    = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],
                        by=list(
                           activityId = finalData2e$activityId
                          ,subjectId = finalData2e$subjectId
                          )
                        ,mean)

#combine with activity type
tidyData    = merge(
                     tidyData
                    ,activityType
                    ,by='activityId'
                    ,all.x=TRUE
                    )

#Export
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')

##########################################################################################################
