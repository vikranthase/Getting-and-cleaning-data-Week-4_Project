
rm(list = ls())


# 1. Merge the training and the test sets to create one data set


setwd('D:/R/Coursera/W4 Project/UCI HAR Dataset')

# Import training data from files 
featuresdata <- read.table('./features.txt',header=FALSE)
activityLabelsdata <- read.table('./activity_labels.txt',header=FALSE)
subjectTraindata <- read.table('./train/subject_train.txt',header=FALSE)
xTraindata <- read.table('./train/x_train.txt',header=FALSE)
yTraindata <- read.table('./train/y_train.txt',header=FALSE)

# Give Name to the columns 
colnames(activityLabelsdata) <- c("activityId","activityType")
colnames(subjectTraindata) <- "subjectId"
colnames(xTraindata) <- featuresdata[,2]
colnames(yTraindata) <- "activityId"

# Merge Data into complete training set
trainingdata = cbind(yTraindata,subjectTraindata,xTraindata)

# Import test data from files 
subjectTestdata <- read.table('./test/subject_test.txt',header=FALSE)
xTestdata <- read.table('./test/x_test.txt',header=FALSE)
yTestdata <- read.table('./test/y_test.txt',header=FALSE)

# Give Name to the columns 
colnames(subjectTestdata) <- "subjectId"
colnames(xTestdata) <- featuresdata[,2]
colnames(yTestdata) <- "activityId"


# Merge Data into complete test set
testdata <- cbind(yTestdata,subjectTestdata,xTestdata)


# Combine Training Data Set and Test Data Set into one Merged Data Set
MergedData <- rbind(trainingdata,testdata)


# Create columns object to prepare data for subsetting
columns <- colnames(MergedData)


# 2. Extract only the measurements on the mean and standard deviation for each measurement.


# Create a object that indentifies the ID, mean & stddev columns as TRUE
identifier <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) & 
             !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
             grepl("-std..",columns) & !grepl("-std()..-",columns))

# Update MergedDataSet based on previously identified columns
MergedData <- MergedData[identifier==TRUE]



# 3. Use descriptive activity names to name the activities in the data set


# Add in descriptive activity names to MergedDataSet & update columns vector
MergedData <- merge(MergedData,activityLabelsdata,by='activityId',all.x=TRUE);
MergedData$activityId <-activityLabelsdata[,2][match(MergedData$activityId, activityLabelsdata[,1])] 

columns <- colnames(MergedData)



# 4. Appropriately label the data set with descriptive variable names

# Tidy column names
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
}

# Update MergedDataSet with new descriptive column names
colnames(MergedData) <- columns

# Remove activityType column
MergedData <- MergedData[,names(MergedData) != 'activityType']



# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Averaging each activity and each subject as Tidy Data
tidyDataset <- aggregate(MergedData[,names(MergedData) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=MergedData$activityId,
                        subjectId=MergedData$subjectId),mean)

# Export tidyData set 
write.table(tidyDataset, './MyTidyData.txt',row.names=FALSE,sep='\t')


