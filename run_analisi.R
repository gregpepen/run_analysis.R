#run_analysis.R
#This script isthe result of completing exercise  for peer-review Week 4th Getting&CleaningData
#submited by merilio morell
# the goal  of the exercise is to prepapre a tidy data set  from  data obtained at 
#"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#This data are part of project aimed at developing means for obtaining information on 
#human  DAILY LIVING activity using embebed smartphones' acceleration and gyroscope sensors
#datails of that projec are available at https://core.ac.uk/download/pdf/41773853.pdf

#SCRIPT SUBMITTED BY MERILIO MORELL

#STEP 1 <setwd()>
setwd("L:/r/PlanRefoRD/data")

#STEP 2 <call required Libraries>
library(data.table)
library(dplyr)

# STEP 3- download data from web
URLpeerreview <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- file.create("./data_train_test.zip")
download.file(URLpeerreview,destfile = "data_train_test.zip", mode='wb')
destfile <-"./data_train_test"

#STEP 4- unzip data
unzip("./data_train_test.zip", exdir = "./data_test_train")

#STEP 5- call data into data table Rstudio
X_test <- read.table("./data_test_train/UCI HAR Dataset/test/X_test.txt", header = F)
y_test <- read.table("./data_test_train/UCI HAR Dataset/test/y_test.txt", header = F )
X_train <- read.table("./data_test_train/UCI HAR Dataset/train/X_train.txt", header = F)
y_train <- read.table("./data_test_train/UCI HAR Dataset/train/y_train.txt", header = F)
subject_test <-  read.table("./UCI HAR Dataset/test/subject_test.txt", header = F)
subject_train <-  read.table("./UCI HAR Dataset/train/subject_train.txt")
features <-  read.table("./UCI HAR Dataset/features.txt", header =  F)
#request number 3 of exercise: USES DESCRIPTIVE NAMES TO NAME ACTIVITIES
features <- as.character(features[,2])

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header =  F) 
#request number 3 of exercise: USES DESCRIPTIVE NAMES TO NAME ACTIVITIES
activity_labels <- as.character(activity_labels[,2])

#STEP 6- merging tables X_test & y_test & subject_test

#STEP 6.1 adding columns *activity* and *subject* to X_test table and to X_train table
X_test_subject <- cbind(X_test,y_test )
X_test_subject_ytest <- cbind(X_test_subject,subject_test) 

X_train_subject <- cbind(X_train, y_train)
X_train_subject_ytrain <- cbind(X_train_subject, subject_train )



names(X_test_subject_ytest) <- c(c(features, 'subject','activity'))
names(X_train_subject_ytrain) <-c(c( features,  'subject','activity'))

#STEP 7 merging Test and Train Tables rows
data_test_train <- rbind(X_train_subject_ytrain, X_test_subject_ytest)

#STEP 8 Subsetting measurements in MEAN and STANDARD DEVIATION
data_test_trainExtract <- data_test_train[,which(colnames(data_test_train)%in% c('subject','activity',grep("mean()|std()",colnames(data_test_train), value=TRUE)))]
                                      
# STEP 9answering request number 3 of exercise: USES DESCRIPTIVE NAMES TO NAME ACTIVITIES
data_test_trainExtract$activity <- activity_labels[data_test_trainExtract$activity]

#step 10 APPROPIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLES NAMES
#10.1 find labels that need a descriptive name
unique(gsub("-(mean|std)().*","",names(data_test_trainExtract)[-c(1:2)]))
#Replace abbreviations of measurement for a descriptive label
# "t" -> time; Acc -< Accelerometer; Gyro -> Gyroscope; "f" -> frequency; Mag -> Magnitude; BodyBody -> Body
names(data_test_trainExtract)[-c(1:2)] <- gsub("^t","time",names(data_test_trainExtract)[-c(1:2)])
names(data_test_trainExtract)[-c(1:2)] <- gsub("^f","frequency",names(data_test_trainExtract)[-c(1:2)])
names(data_test_trainExtract)[-c(1:2)] <- gsub("^Acc","Accelerometer",names(data_test_trainExtract)[-c(1:2)])
names(data_test_trainExtract)[-c(1:2)] <- gsub("^Gyro","Gyroscope",names(data_test_trainExtract)[-c(1:2)])
names(data_test_trainExtract)[-c(1:2)] <- gsub("^Mag","Magnitude",names(data_test_trainExtract)[-c(1:2)])
names(data_test_trainExtract)[-c(1:2)] <- gsub("^BodyBody","Body",names(data_test_trainExtract)[-c(1:2)])

#FROM DATA IN STEP 4 CREATE SECOND INDEPENDEN TIDY DATA SET WITH AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY
tidydata <- aggregate(.~subject + activity, data_test_trainExtract, mean)
tidydata <- tidydata[order(tidydata$subject,tidydata$activity),]
tidydata
dim(tidydata)
#[1] 36 81
dim(data_test_trainExtract)
#[1] 10299    81
write.table(tidydata, file = "./data/tidydata.txt", sep = ",")
