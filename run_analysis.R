
library(dplyr)



# 1. Adquiring data
# 1.1  Download the file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

#  1.2 Unzip the file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# 1.3 Know the the list of the unzipped files extracted at UCI HAR Dataset folder
path_to_ds <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_to_ds, recursive=TRUE)
length(files)

# 1.4 Doing all the necessary data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")





#---------------------------------------------------------------------------

# 2. TASK 1: Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subjects <- rbind(subject_train, subject_test)
Data_Merged <- cbind(subjects, Y, X)






#----------------------------------------------------------------------------------------------------

# 3. TASK 2: Extracts only the measurements on the mean and standard deviation for each measurement. 

Tidy_Data <- Data_Merged %>% select(subject, code, contains("mean"), contains("std"))

#---------------------------------------------------------------------------------------------------

# 4 TASK 3 Uses descriptive activity names to name the activities in the data set.
Tidy_Data$code <- activity_labels[Tidy_Data$code, 2]

names(Tidy_Data)[2] = "activity"
names(Tidy_Data)<-gsub("Acc", "Accelerometer", names(Tidy_Data))
names(Tidy_Data)<-gsub("Gyro", "Gyroscope", names(Tidy_Data))
names(Tidy_Data)<-gsub("BodyBody", "Body", names(Tidy_Data))
names(Tidy_Data)<-gsub("Mag", "Magnitude", names(Tidy_Data))
names(Tidy_Data)<-gsub("^t", "Time", names(Tidy_Data))
names(Tidy_Data)<-gsub("^f", "Frequency", names(Tidy_Data))
names(Tidy_Data)<-gsub("tBody", "TimeBody", names(Tidy_Data))
names(Tidy_Data)<-gsub("-mean()", "Mean", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-std()", "STD", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("-freq()", "Frequency", names(Tidy_Data), ignore.case = TRUE)
names(Tidy_Data)<-gsub("angle", "Angle", names(Tidy_Data))
names(Tidy_Data)<-gsub("gravity", "Gravity", names(Tidy_Data))

# 5. TASK 4 From the data set in step 4, creates a second, independent tidy data set with the average of 
#each variable for each activity and each subject.

FinalData <- Tidy_Data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
#tidy data set
write.table(FinalData, "Tidy_dataset.txt", row.name=FALSE)
#---------------------------------------------------------------------------------------------------

# 6 CHECKING  
class(FinalData)
#[1] "grouped_df" "tbl_df"     "tbl"        "data.frame"

str(FinalData)
#tibble [180 × 88] (S3: grouped_df/tbl_df/tbl/data.frame)
# $ subject                                           : int [1:180] 1 1 1 1 1 1 2 2 2 2 ...
# $ activity                                          : Factor w/ 6 levels "LAYING","SITTING",..: 1 2 3 4 5 6 1 2 3 4 ...
# $ TimeBodyAccelerometer.mean...X                    : num [1:180] 0.222 0.261 0.279 0.277 0.289 ...
# $ TimeBodyAccelerometer.mean...Y                    : num [1:180] -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 .

FinalData
# A tibble: 180 x 88
# Groups:   subject [30]
#   subject activity TimeBodyAcceler… TimeBodyAcceler… TimeBodyAcceler… TimeGravityAcce… TimeGravityAcce… TimeGravityAcce…
#     <int> <fct>               <dbl>            <dbl>            <dbl>            <dbl>            <dbl>            <dbl>
# 1       1 LAYING              0.222         -0.0405           -0.113            -0.249            0.706           0.446 
# 2       1 SITTING             0.261         -0.00131          -0.105             0.832            0.204           0.332 
# 3       1 STANDING            0.279         -0.0161           -0.111             0.943           -0.273           0.0135
# 4       1 WALKING             0.277         -0.0174           -0.111             0.935           -0.282          -0.0681
# 5       1 WALKING…            0.289         -0.00992          -0.108             0.932           -0.267          -0.0621
# 6       1 WALKING…            0.255         -0.0240           -0.0973            0.893           -0.362          -0.0754
# 7       2 LAYING              0.281         -0.0182           -0.107            -0.510            0.753           0.647 
# 8       2 SITTING             0.277         -0.0157           -0.109             0.940           -0.106           0.199 
# 9       2 STANDING            0.278         -0.0184           -0.106             0.897           -0.370           0.130 
#10       2 WALKING             0.276         -0.0186           -0.106             0.913           -0.347           0.0847
# … with 170 more rows, and 80 more variables:

