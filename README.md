# Getting-and-cleaning-data

This repo was created to finish the assignment for week 4 of Getting and Cleaning Data Coursera course.


## Data description

The variables in the data X are sensor signals measured with waist-mounted smartphone from 30 subjects. The variable in the data Y indicates activity type the subjects performed during recording.

## Code explaination

The code combined training dataset and test dataset, and extracted partial variables to create another dataset with the averages of each variable for each activity.

## New tidy dataset

The new generated dataset contained variables calculated based on the mean and standard deviation. Each row of the dataset is an average of each activity type for all subjects.

## Description of the Code of run_analysis.R

1. Download the zip file provided in the assignment.
2. Unzip the file in the Working directory
3. Read training and testing dataset into R environment. Read variable names into R envrionment. Read subject index into R environment.
4. Merges the training and the test sets to create one data set. Use command rbind to combine training and testing set
5. Extracts only the measurements on the mean and standard deviation for each measurement. Use grep command to get column indexes for variable name contains "mean()" or "std()"
6. Uses descriptive activity names to name the activities in the data set Convert activity labels to characters and add a new column as factor
7. Appropriately labels the data set with descriptive variable names. Give the selected descriptive names to variable columns
8. From the data set in step 7, creates a second, independent tidy data set with the average of each variable for each activity and each subject. Use pipeline command to create a new tidy dataset with command group_by and summarize_each in dplyr package