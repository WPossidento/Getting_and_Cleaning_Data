## Getting and Cleaning Data
## Course Project

## run_analysis.R

## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.

# Set working directory:
setwd("C:/Users/Bill/Desktop/Coursera_JHU_Data_Science_Specialization/UCI HAR Dataset")

# Read the data from the files:
features = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Name the columns:
colnames(activityType) = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activityId";

# Create the final training set by merging yTrain, subjectTrain, and xTrain:
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read the test data:
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Name the test data columns:
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2];
colnames(yTest) = "activityId";

# Merge the xTest, yTest and subjectTest data to create a single data set:
testData = cbind(yTest,subjectTest,xTest);

# Merge the training and test data to create a single data set:
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData to select the 
# desired mean() & stddev() columns:
colNames = colnames(finalData);

# 2. Extract only the measurements on the mean and standard deviation for each measurement:

# Define logicalVector to TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData to retain desired columns:
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set:

# Merge the finalData set with the activityType table to include descriptive activity names:
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Update the colNames vector to include the new column names after merge:
colNames = colnames(finalData);

# 4. Appropriately label the data set with descriptive activity names:

# Clarify the column names:
for (i in 1:length(colNames)) {
    colNames[i] = gsub("-mean","Mean",colNames[i])    
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
};

# Assign the clarified column names to the finalData set:
colnames(finalData) = colNames;

# 5.  From the data set in step 4, creates a second, independent tidy data set with 
#     the average of each variable for each activity and each subject:

# Create a new table, finalDataNoActivityType, excluding activityType:
finalDataNoActivityType = finalData[,names(finalData) != 'activityType'];

# Summarize finalDataNoActivityType with just the mean of each variable for each activity and each subject:
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merge tidyData with activityType to include descriptive acitvity names:
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export tidyData:
write.table(tidyData, './tidyData.txt',row.name=FALSE,sep='\t');