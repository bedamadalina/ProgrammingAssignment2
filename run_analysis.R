#Madi Coursera Project
#what I have to do
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# # From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# I will do
# Merging the training and the test sets to create one data set.
# 1.1 Reading files
# 1.1.1 Reading trainings tables
# 1.1.2 Reading testing tables
# 1.1.3 Reading feature vector
# 1.1.4 Reading activity labels
# 1.2 Assigning column names
# 1.3 Merging all data in one set
# Extracting only the measurements on the mean and standard deviation for each measurement
# 2.1 Reading column names
# 2.2 Create vector for defining ID, mean and standard deviation
# 2.3 Making nessesary subset from setAllInOne
# Using descriptive activity names to name the activities in the data set
# Appropriately labeling the data set with descriptive variable names
# Creating a second, independent tidy data set with the average of each variable for each activity and each subject
# 5.1 Making second tidy data set
# 5.2 Writing second tidy data set in txt file

# download and unzip
install.packages('reshape2')
library(reshape2)


# Initialize some initial values
targetFolder <- 'UCI HAR Dataset'
filename <- 'getdata_dataset.zip'

# Check if the user has already unzipped the file
if(!file.exists(targetFolder)) {
  # Do they at least have the zip file
  if(!file.exists(filename)) {
    
    #  so I download it
    download.file(
      'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
      filename
    )
  }
  
  # Now, unzip the file
  unzip(filename)
}

# 1. Merges the training and the test sets to create one data set.

# Read in the data into the test and training sets
test.data <- read.table(file.path(targetFolder, 'test', 'X_test.txt'))
test.activities <- read.table(file.path(targetFolder, 'test', 'y_test.txt'))
test.subjects <- read.table(file.path(targetFolder, 'test', 'subject_test.txt'))

train.data <- read.table(file.path(targetFolder, 'train', 'X_train.txt'))
train.activities <- read.table(file.path(targetFolder, 'train', 'y_train.txt'))
train.subjects <- read.table(file.path(targetFolder, 'train', 'subject_train.txt'))

# Bind the rows for each of the data sets together
data.data <- rbind(train.data, test.data)
data.activities <- rbind(train.activities, test.activities)
data.subjects <- rbind(train.subjects, test.subjects)

# Now combine all of of the different columns together into one table
full_data <- cbind(data.subjects, data.activities, data.data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Grab the complete list of features
features <- read.table(file.path(targetFolder, 'features.txt'))

# Filter to the features we want
requiredFeatures <- features[grep('-(mean|std)\\(\\)', features[, 2 ]), 2]
full_data <- full_data[, c(1, 2, requiredFeatures)]

# 3. Uses descriptive activity names to name the activities in the data set

# Read in the activity labels
activities <- read.table(file.path(targetFolder, 'activity_labels.txt'))

# Update the activity name
full_data[, 2] <- activities[full_data[,2], 2]

# 4. Appropriately labels the data set with descriptive variable names. 

colnames(full_data) <- c(
  'subject',
  'activity',
  # Remove the brackets from the features columns
  gsub('\\-|\\(|\\)', '', as.character(requiredFeatures))
)

# Coerce the data into strings
full_data[, 2] <- as.character(full_data[, 2])

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Melt the data so we have a unique row for each combination of subject and acitivites
final.melted <- melt(full_data, id = c('subject', 'activity'))

# Cast it getting the mean value
final.mean <- dcast(final.melted, subject + activity ~ variable, mean)

# Emit the data out to a file
write.table(final.mean, file=file.path("tidy.txt"), row.names = FALSE, quote = FALSE)

library(knitr)
knit2html("codebook.md");

