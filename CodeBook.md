#CODE BOOK for Getting and Cleaning Data - Course Project

#The UCI HAR Dataset (raw data)

The UCI HAR Dataset represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available in the "README.txt"" file availlable in the "UCI HAR Dataset" folder in the current repo, as much as at this website: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#Project Assignments

The project required to start from the UCI HAR Dataset and:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

All these tranformations are performed through the 'run_analysis.R' script in the repo that saves the "Tidy data set.txt" in the main folder. For more info on how to correctly apply it, please refer to the "README.md" file.

#Feature explanations and selection for the tidy data set

In the "UCI HAR Dataset" folder, the "features_info.txt"" file provides the following description about the original feature availlable in the data set:

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

In the original data, several variables were estiomated from the preivous signals but the Course Project assigment asked to extract in the tidy data set only the mean and standard deviation for each measurement, that are:

mean(): Mean value
std(): Standard deviation

Due to a possible ambiguity in the assigment and in order not to leave out relevant variable, The following variable was extracted in the tidy data:

meanFreq(): Weighted average of the frequency components to obtain a mean frequency

There are also additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable. As these are mean value, also these were exctracted in the tidy data set:

* X,gravityMean
* Y,gravityMean
* Z,gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

All features are normalized and bounded within [-1,1].

#Variables in the Tidy data set

The tidy data set generated by the script will be saved as "Tidy data set.txt" in the main folder.

V1 "Subject": 
It contains the ID of the subject

V2 "Activity": 
It indicates which of the 6 different activities the data refers to. The 6 activities are:
  - "WALKING"
  - "WALKING_UPSTAIRS"
  - "WALKING_DOWNSTAIRS" 
  - "SITTING"            
  - "STANDING"          
  - "LAYING"

V3-V88:
As a result of the cleaneup of the original data set with the application of the 'run_analysis.R' script, the tidy data set will contain only 86 from the original features, i.e. only those regarding mean and standard deviation for each measurement and the 6 final additional angle varianbles.

It was also required that the Tidy data set contains the average of each included variable for each activity and each subject, as each subjectes repeated each activity several times.

The name of the variables mantain the original naming found in the UCI HAR Dataset. 
V3-V81 follow the naming convention of "signal"-"variable"-"axis", e.g V3 = "tBodyAcc-mean()-X"", V7 = "tBodyAcc-std()-Y" and V57 = "fBodyAccJerk-meanFreq()-Z". 
V82-V88 instead follow the naming convention of angle("feature"), e.g. V88 = angle(Z,gravityMean).

