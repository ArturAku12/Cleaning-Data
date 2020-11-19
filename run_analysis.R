#Downloading necessary packages for combination of data
library(data.table)
library(dplyr)

# Checks if a data file is created, if not
# then a data file is created for storage
# and organisation of the data frames

if(!file.exists("data")) {
  dir.create("data")
}

# Downloading the data and unzipping it
url_zip <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url_zip, destfile = "./data/UCI_HAR_Dataset.zip", method = "curl")
dateDownloaded <- date()

# Combining the test and training sets and combining them
test_x <- read.table("./data/UCI HAR Dataset/test/X_test.txt") 
train_x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
subjects_data <- rbind(test_x, train_x) 

# Combining the activities labels for test and training sets and combining them
label_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
label_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
label_data <- rbind(label_test,label_train)

# Getting and combining the labels for participants from the test and training sets
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subject_label_data <- rbind(subject_test,subject_train)

#Renaming the column in subject_label_data
colnames(subject_label_data) <- "Subject-Number"

# Activity labels that translates the numerical value given in label_data to 
# descriptive
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

# Renaming the columns in label_data to better describe the data using the
# label descriptions in activity_labels
names(activity_labels) <- c("Activity-Number", "Activity-Label") #renames the 2 columns in activity_labels
names(label_data) <- "Activity-Number" #renames the single column in label_data

# Creates a new table called activity_data, using left_join to preserve all the rows 
# present in label_data whilst renaming it using the info from activity_labels. By using
# [,2], we then choose the second column which has all the right names without any numbers.
# NB: it is important to note the use of 'left_join' as opposed to 'right_join', as the use
# of the former preserves the order of the labels rather than going in the ascending numerical
# order. This is important as to preserve the right order of data entries across all the tables.
activity_data <- left_join(label_data, activity_labels, by = "Activity-Number")
activity_data <- activity_data[,2]


# Features file that contains the column names for all 561 measurements and 
features_data <- read.table("./data/UCI HAR Dataset/features.txt")

# Replacing all the column names in subjects_data with the better descriptive labels
# from features_data file
colnames_data <- features_data$V2 #extracts the features names and places it into a variable
colnames(subjects_data) <- colnames_data #renames all the columns in the table

# Having appropriately given all the labels a description and attaching names to the table of
# observations it is possible to start with combining all three tables together into one.
data_table <- cbind(subject_label_data, activity_data) #Combining subject label and activity label
data_table <- cbind(data_table, subjects_data) #Combining the labels with the bulk of the data

# Finding and extracting all the columns that measure mean() or std() + the first two columns
names_dt <- names(data_table) #extract names of columns
cols_dt_location <- grep("mean\\(\\)|std\\(\\)", names_dt) #finds the indexes of columns which have std() or mean()
data_table <- data_table[, c(1,2, cols_dt_location)] #gets all the desired columns

# Renaming the columns by using gsub
names(data_table) <- gsub("-", " ", names(data_table))
names(data_table) <- gsub("_", " ", names(data_table))
names(data_table) <- gsub("^t", "Time ", names(data_table))
names(data_table) <- gsub("^f", "Frequency ", names(data_table))
names(data_table) <- gsub("Acc", "Accelerometer ", names(data_table))
names(data_table) <- gsub("Body", "Body ", names(data_table))
names(data_table) <- gsub("Mag", "Magnitude ", names(data_table))
names(data_table) <- gsub("Gravity", "Gravity ", names(data_table))
names(data_table) <- gsub("Gyro", "Gyroscope ", names(data_table))

#Creating a data set which creates an average for every variable for each activity and subject
# using the aggregate function
tidy_data <- aggregate(. ~`Subject Number` + `activity data`, data_table, mean)

#Writing a file with tidy_data, with row.names as false as to not use the indexes
#as row names
write.table(tidy_data, file = "tidy_data_set.txt", row.names = FALSE)
