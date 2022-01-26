## You should create one R script called run_analysis.R that does the following.
##
## 1. Merges the training and the test sets to create one data set.
## - assumes data has been downloaded and saved to the working directory
## - files to be read 
##   - x_train, y_train, subject_train from train folder 
##   - x_test, y_test , subject_test from test folder
##   - features.txt and activity_labels.txt for identifiers

## load required library
library(dplyr)

## read files
features <- read.table("features.txt", col.names = c("No.", "Feature"))
activities <- read.table("activity_labels.txt", col.names = c("ID", "Activity"))

xtrain <- read.table("./train/x_train.txt", col.names = features[, 2])
ytrain <- read.table("./train/y_train.txt", col.names = "ID")
strain <- read.table("./train/subject_train.txt", col.names = "Subject")

xtest <- read.table("./test/x_test.txt", col.names = features[, 2])
ytest <- read.table("./test/y_test.txt", col.names = "ID")
stest <- read.table("./test/subject_test.txt", col.names = "Subject")

## combine training data
trainset <- cbind(strain, ytrain, xtrain)

## combine test data
testset <- cbind(stest, ytest, xtest)

## merge train and test data
mergeddata <- rbind(trainset, testset)

## - read activity_labels and features
colnames(mergeddata) <- c("Subject", "Activity", features[, 2])
mergeddata$Activity <- activities[mergeddata$Activity, 2] 

## 2. Extracts only the measurements on the mean and standard deviation for each
##    measurement
## - extract only with matching mean() and std()
mean_and_std_data <- mergeddata %>% select(Subject, Activity, contains("mean"), contains("std"))


## 3. Uses descriptive activity names to name the activities in the data set
## -  completed in previous step when reading activity_labels

## 4. Appropriately labels the data set with descriptive variable names.
 
names(mean_and_std_data) <- gsub("Acc", "Accelerometer", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("Gyro", "Gyroscope", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("Mag", "Magnitude", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("^t", "Time", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("^f", "Frequency", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("-mean()", "Mean", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("-std()", "StdDeviation", names(mean_and_std_data))
names(mean_and_std_data) <- gsub("-freq-", "Frequency", names(mean_and_std_data))


## 5. From the data set in step 4, creates a second, independent tidy data set with the
## average of each variable for each activity and each subject.

ave_data <- mean_and_std_data %>%
        group_by(Subject, Activity) %>%
        summarise_all(funs(mean))

write.table(ave_data, "ave_data.txt", row.name=FALSE)
