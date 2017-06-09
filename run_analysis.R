library(dplyr)

#Assign a name to the file
filename <- "get_clean_data.zip"

# Check if filename already exists
if(!file.exists(filename)) {
  # Download the zip file
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, filename, method = "curl")
}

# Check if file exists
if(!file.exists("UCI HAR Dataset")) {
  #Unzip the file
  unzip(filename)
}

# read the training data
training_X <- read.table("UCI HAR Dataset/train/X_train.txt")
training_Y <- read.table("UCI HAR Dataset/train/Y_train.txt")
training_Sub <- read.table("UCI HAR Dataset/train/subject_train.txt")

# read the testing data
testing_X <- read.table("UCI HAR Dataset/test/X_test.txt")
testing_Y <- read.table("UCI HAR Dataset/test/Y_test.txt")
testing_Sub <- read.table("UCI HAR Dataset/test/subject_test.txt")

# read the data description to variable names
var_names <- read.table("UCI HAR Dataset/features.txt")

# read activity labels
act_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

#1. Merge the training and the test sets to create one data set.
total_X <- rbind(training_X ,testing_X)
total_Y <- rbind(training_Y, testing_Y)
total_Sub <- rbind(training_Sub, testing_Sub)

#2. Extract only the measurements on the mean and standard deviation for each measurement.
select_var <- var_names[grep("mean\\(\\)|std\\(\\)",var_names[,2]),]
total_X <- total_X[,select_var[,1]]

#3. Use descriptive activity names to name the activities in the data set
colnames(total_Y) <- "activity"
total_Y$activitylabel <- factor(total_Y$activity, labels = as.character(act_labels[,2]))
activitylabel <- total_Y[,-1]

#4. Appropriately labels the data set with descriptive variable names.
colnames(total_X) <- var_names[select_var[,1],2]

#5. From the data set in step 4, create a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(total_Sub) <- "subject"
total <- cbind(total_X, activitylabel, total_Sub)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
#Upload tidydata file with the final tidy data
write.table(total_mean, file = "UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)

