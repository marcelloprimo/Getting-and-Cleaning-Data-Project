## ASSIGNMENT: 0.   You should create one R script called run_analysis.R that does the following. 
#                   Here are the data for the project:

# define URL
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


# download file
download.file(fileUrl,destfile = "./Data.zip")


# unzip file
unzip(zipfile="./Data.zip")


# define file path to unzipped data
filePath <- file.path("./" , "UCI HAR Dataset")


# open train set
trainSet <- read.table(file.path(filePath, "train", "X_train.txt"),header = FALSE)

# trainSet is a data.frame, where the 561 variables are the features and the 7352 observations are made
# on 21 subjects (equal to the 70% of the total 30 subjects participating to the experiment):
#    > str(trainSet)
#    'data.frame':     7352 obs. of  561 variables:
#         $ V1  : num  0.289 0.278 0.28 0.279 0.277 ...
#    [list output truncated]


# open test set
testSet <- read.table(file.path(filePath, "test", "X_test.txt"),header = FALSE)

# testSet is a data.frame, where the 561 variables are the features and the 2947 observations are made
# on 9 subjects (equal to the 30% of the total 30 subjects participating to the experiment):
#    > str(testSet)
#    'data.frame':     2947 obs. of  561 variables:
#         $ V1  : num  0.257 0.286 0.275 0.27 0.275 ...
#    [list output truncated]



## ASSIGNMENT: 1.   Merges the train and the test sets to create one data set.

# merge sets by rows, first train then test
allSet <- rbind(trainSet,testSet)


# assigns the pre-definited 561 features names to the 561 variables of the above allSet

# reads names of features
featuresNames <- read.table(file.path(filePath,  "features.txt"),header = FALSE)

# featuresNames$V2 contains the names of the features
#    > str(featuresNames)
#    'data.frame':     561 obs. of  2 variables:
#         $ V1: int  1 2 3 4 5 6 7 8 9 10 ...
#    $ V2: Factor w/ 477 levels "angle(tBodyAccJerkMean),gravityMean)",..: 243 244 245 250 251 252 237 238 239 240 ...


# assigns names
names(allSet) <- featuresNames$V2



## ASSIGNMENT: 2.   Extracts only the measurements on the mean and standard deviation for each measurement. 

# extracts feature names containg substrings "mean" or "std"
part_featuresNames<-featuresNames$V2[grep("mean|std", featuresNames$V2)]
                    

# creates vector of desired features names
vec_part_featuresNames <- as.vector(part_featuresNames)

# There are 79 features relative to mean and standard deviation:
#    > str(vec_part_featuresNames)
#    chr [1:79] "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z" "tBodyAcc-std()-X" "tBodyAcc-std()-Y" "tBodyAcc-std()-Z" ...


# extracts desired columns from allSet
part_allSet <- allSet[,vec_part_featuresNames]

# part_allSet is a dataframe composed of only the desired columns and containing the total number of records
#    > str(part_allSet)
#    'data.frame':     10299 obs. of  79 variables:
#         $ tBodyAcc-mean()-X              : num  0.289 0.278 0.28 0.279 0.277 ...
#    [list output truncated]
                    
                    
                    
## ASSIGNMENT: 3.   Uses descriptive activity names to name the activities in the data set
                    
# reads activities names
activityNames <- read.table(file.path(filePath,  "activity_labels.txt"),header = FALSE)
                    
#    > str(activityNames)
#    'data.frame':     6 obs. of  2 variables:
#    $ V1: int  1 2 3 4 5 6
#    $ V2: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1
                    
                    
# reads train and test activities
train_activitySet <- read.table(file.path(filePath, "train", "y_train.txt"),header = FALSE)
test_activitySet <- read.table(file.path(filePath, "test", "y_test.txt"),header = FALSE)
                    
                    
# merges sets by rows
all_activitySet <- rbind(train_activitySet,test_activitySet)
                    
                    
# Uses descriptive activity names to name the activities in the data set
all_activitySet <-as.vector(sapply(all_activitySet$V1,FUN = function(x) {activityNames$V2[activityNames$V1==x]}))
                    
                    
# reads subjects train and test files
train_subject_set <- read.table(file.path(filePath, "train", "subject_train.txt"),header = FALSE)                    
test_subject_set <- read.table(file.path(filePath, "test", "subject_test.txt"),header = FALSE)                    

                    
# merges sets by rows
all_subjectSet <- rbind(train_subject_set,test_subject_set)
                    
                    
# merges all the sets by columns
all_Data <- cbind(part_allSet,all_activitySet,all_subjectSet)
                    
                    
                    
## ASSIGNMENT: 4.   Appropriately labels the data set with descriptive variable names.

library("dplyr")                   

                    
# names variable
all_Data <- rename(all_Data,subject=V1,activity=all_activitySet)

                    
                    
## ASSIGNMENT: 5.   From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

TidyData<-aggregate(. ~ activity +subject,all_Data,function(x) mean = mean(x))

                    
write.table(TidyData, file = "tidydata.txt",row.name=FALSE)