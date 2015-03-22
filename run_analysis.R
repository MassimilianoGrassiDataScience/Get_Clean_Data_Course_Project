
#----1 Merges the training and the test sets to create one data set of S, X and Y.

xtr <- read.table("./UCI HAR Dataset/train/X_train.txt")
xte <- read.table("./UCI HAR Dataset/test/X_test.txt")
X <- rbind(xtr, xte)

str <- read.table("./UCI HAR Dataset/train/subject_train.txt")
ste <- read.table("./UCI HAR Dataset/test/subject_test.txt")
S <- rbind(str, ste)

ytr <- read.table("./UCI HAR Dataset/train/y_train.txt")
yte <- read.table("./UCI HAR Dataset/test/y_test.txt")
Y <- rbind(ytr, yte)

#----2 Extracts only the measurements on the mean and standard deviation for each measurement in X.

#read features
features <- read.table("./UCI HAR Dataset/features.txt")

#select features of interest (mean and standard deviation)
features_of_interest_mean <- grep('mean', features[, 2], ignore.case=TRUE)
features_of_interest_std <- grep('std', features[, 2], ignore.case=TRUE)
features_of_interest_index  <- c(features_of_interest_mean, features_of_interest_std)
features_of_interest_index <- sort(features_of_interest_index)

#create new dataset with only features of interest (mean and standard deviation)
X_mean_std  <- X[,features_of_interest_index]

#----3 Use descriptive activity names to name the activities in the data set.

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
activities[,2]  <- as.character(activities[,2])
Y[,1]  <- as.character(Y[,1])
Y[,1]  <- activities[Y[,1],2]

#----4 Appropriately labels the data set with descriptive activity names.

# create a merged and unique dataset
dataset <- tbl_df(bind_cols(S,Y,X_mean_std))

#Rename variable names with feature names
features_of_interest_names  <- as.character(features[features_of_interest_index,2])
names(dataset) <- c('Subject', 'Activity', features_of_interest_names)

#5 Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

Subject  <- unique(dataset$Subject)
NumSubject  <- length(Subject)
Activity  <- unique(dataset$Activity)
NumActivity <- length(unique(Activity))
Dim  <- dim(dataset)
Cols  <- Dim[2]
Tidy_dataset  <- dataset[1:(NumSubject*NumActivity), ]

library("dplyr")

rownum = 1
for (s in 1:NumSubject) {
  for (a in 1:NumActivity) {
    Tidy_dataset[rownum, 1] = Subject[s]
    Tidy_dataset[rownum, 2] = Activity[a]
    data <- filter(dataset, Subject==Subject[s], Activity==Activity[a])
    Tidy_dataset[rownum, 3:Cols] <- colMeans(data[, 3:Cols])
    rownum = rownum+1
  }
}

#save tidy data as .txt file
write.table(Tidy_dataset, "Tidy data set.txt")
