# course 3 : getting and cleaning data 
# week 4 : course project 
##Human Activity Recognition Using Smartphones Dataset

#unzip data to the directory 
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "./data/getdata_projectfiles_UCI HAR Dataset.zip")
unzip(zipfile = "./data/getdata_projectfiles_UCI HAR Dataset.zip", exdir =  "./data")
#1 - read and merge the data 
# reading the data 
  ## data details 
  FeaturesNames <- read.table("./data/UCI HAR Dataset/features.txt")
  ActivityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
  colnames(ActivityLabels) <- c("Activity_ID", "Activity_Type")
 ## train data 
  train_set <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  train_labels <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
 ## test data 
  test_set <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  test_labels <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
 ## merge data 
  ## set
  sets_data <- rbind(train_set,test_set)
  colnames(sets_data) <- FeaturesNames$V2 #name columns
  ## labels
  labels_data <- rbind(train_labels,test_labels)
  colnames(labels_data) <- "Activity_ID" # name the columns 
  ## subjects
  subjects_data <- rbind(subject_test,subject_train)
  names(subjects_data) <- "Subject_ID"   ## name the columns 
## Merging all data 
  all_data <- cbind(labels_data,subjects_data,sets_data)
#2- Extracts only the measurements on the mean and standard deviation for each measurement
  subFeaturesNames <- FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", FeaturesNames$V2)]
  DataNames <- c("Subject_ID", "Activity_ID", as.character(subFeaturesNames))
  DataSet_mean_Sd <- subset(all_data, select=DataNames)
  
#3-Uses descriptive activity names to name the activities in the data set
  #method 1 
  setWithActivityNames1 <- merge(DataSet_mean_Sd, ActivityLabels,
                                by = "Activity_ID")
  #method2
  ###################################################
  setWithActivityNames <- DataSet_mean_Sd %>%
    mutate(
      Activity_Type = case_when(
        Activity_ID == 1 ~ "WALKING",
        Activity_ID == 2 ~ "WALKING_UPSTAIRS",
        Activity_ID == 3 ~ 'WALKING_DOWNSTAIRS',
        Activity_ID == 4 ~ "SITTING",
        Activity_ID == 5 ~ "STANDING",
        Activity_ID == 6 ~ "LAYING",
      )
    )
  #################################################
  
  col_idx <- grep("Activity_Type", names(setWithActivityNames1))
  setWithActivityNames1 <- setWithActivityNames1[, c(col_idx, (1:ncol(setWithActivityNames1))[-col_idx])]

#4 
  #done with 1 
  
  data_with_ActivityNames <- merge(all_data, ActivityLabels,
              by = "Activity_ID")
  col_idx <- grep("Activity_Type", names(data_with_ActivityNames))
  data_with_ActivityNames <- data_with_ActivityNames[, c(col_idx, (1:ncol(b1))[-col_idx])]
  data_with_ActivityNames <- subset(data_with_ActivityNames,select = -Activity_ID)
#5-create new data set
  SecondDataSet<-aggregate(. ~Subject_ID + Activity_Type, data_with_ActivityNames, mean)
  SecondDataSet<-SecondDataSet[order(SecondDataSet$Subject_ID,SecondDataSet$Activity_Type),]
  write.table(SecondDataSet,"data_with_mean_ActivityNames.txt",row.names = FALSE)
  