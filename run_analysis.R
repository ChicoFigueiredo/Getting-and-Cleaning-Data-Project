
##############################################################################
# Step 01: Set the line below to work folder
setwd("D:/Estudar/#Cientista_Dados/00.Curso.Projetos/Getting-and-Cleaning-Data-Project")

##############################################################################
# Step 02: Instaling packages and loading (uncomment for install)
# Installing requeried packages
# install.packages("data.table")
# install.packages("reshape2")
# install.packages(dplyr)
library(data.table)
library(reshape2)
library(utils)
library(dplyr)


##############################################################################
# Step 03: Get current dir
path <- getwd()
path

##############################################################################
# Step 04: Download zip data
url.data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip.folder <- file.path(path, "zip")
if (!file.exists(zip.folder)) {
  dir.create(zip.folder)
}
f <- "Dataset.zip"
zip.file <- file.path(zip.folder, f)
download.file(url.data, zip.file)


##############################################################################
# Step 05: Extract zip data
data.folder <- file.path(path, "data")
if (!file.exists(data.folder)) {
  dir.create(data.folder)
}
unzip(zip.file,exdir = data.folder)

# List Files for conference
uci.har.dataset <- file.path(data.folder,"UCI HAR Dataset")
list.files(uci.har.dataset, recursive = TRUE)



##############################################################################
# Step 06: Read the training data from archives
training.Subjects <- read.table(file.path(uci.har.dataset, "train", "subject_train.txt"))
training.Values <- read.table(file.path(uci.har.dataset, "train", "X_train.txt"))
training.Activity <- read.table(file.path(uci.har.dataset, "train", "y_train.txt"))



##############################################################################
# Step 07: Read the test data from archives
test.Subjects <- read.table(file.path(uci.har.dataset, "test", "subject_test.txt"))
test.Values <- read.table(file.path(uci.har.dataset, "test", "X_test.txt"))
test.Activity <- read.table(file.path(uci.har.dataset, "test", "y_test.txt"))

##############################################################################
# Step 08: Read features, don't convert text labels to factors
features <- read.table(file.path(uci.har.dataset, "features.txt"), as.is = TRUE)


##############################################################################
# Step 09: Read activity labels
activities <- read.table(file.path(uci.har.dataset, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##############################################################################
# Step 10: Merge the training and the test sets to create one data set
# concatenate individual data tables to make single data table
human.Activity <- rbind(
  cbind(training.Subjects, training.Values, training.Activity),
  cbind(test.Subjects, test.Values, test.Activity)
)

# remove individual data tables to save memory
rm(training.Subjects, training.Values, training.Activity, 
   test.Subjects, test.Values, test.Activity)

##############################################################################
# Step 11: Assign column names
colnames(human.Activity) <- c("subject", features[, 2], "activity")


##############################################################################
# Step 11: Extract only the measurements on the mean and standard deviation
# for each measurement and determine columns of data set to keep based on column name...
columns.To.Keep <- grepl("subject|activity|mean|std", colnames(human.Activity))

# ... and keep data in these columns only
human.Activity <- human.Activity[, columns.To.Keep]


##############################################################################
# Step 12: Use descriptive activity names to name the activities in the data
#          set
# replace activity values with named factor levels
human.Activity$activity <- factor(human.Activity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


##############################################################################
# Step 13: Appropriately label the data set with descriptive variable names
# get column names
human.Activity.Cols <- colnames(human.Activity)

##############################################################################
# Step 14: remove special characters
human.Activity.Cols <- gsub("[\\(\\)-]", "", human.Activity.Cols)

##############################################################################
# Step 15: expand abbreviations and clean up names
human.Activity.Cols <- gsub("^f", "frequencyDomain", human.Activity.Cols)
human.Activity.Cols <- gsub("^t", "timeDomain", human.Activity.Cols)
human.Activity.Cols <- gsub("Acc", "Accelerometer", human.Activity.Cols)
human.Activity.Cols <- gsub("Gyro", "Gyroscope", human.Activity.Cols)
human.Activity.Cols <- gsub("Mag", "Magnitude", human.Activity.Cols)
human.Activity.Cols <- gsub("Freq", "Frequency", human.Activity.Cols)
human.Activity.Cols <- gsub("mean", "Mean", human.Activity.Cols)
human.Activity.Cols <- gsub("std", "StandardDeviation", human.Activity.Cols)

# correct typo
human.Activity.Cols <- gsub("BodyBody", "Body", human.Activity.Cols)

##############################################################################
# Step 16: Use new labels as column names
colnames(human.Activity) <- human.Activity.Cols


##############################################################################
# Step 17: Create a second, independent tidy set with the average of each
# variable for each activity and each subject
# group by subject and activity and summarise using mean
human.Activity.Means <- human.Activity %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

##############################################################################
# Step 18: output to file "tidy_data.txt", conform instructions
write.table(human.Activity.Means, 
            "tidy_data.txt", 
            row.names = FALSE, 
            quote = FALSE,
            sep = "|")
