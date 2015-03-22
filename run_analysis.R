run_analysis <- function(){
  
  currentDir <- getwd()
  # Read the training and test sets 
  xTrain <- read.table(paste(currentDir,"/train/X_train.txt",sep=""))
  xTest <- read.table(paste(currentDir,"/test/X_test.txt",sep=""))
  yTrain <- read.table(paste(currentDir,"/train/Y_train.txt",sep=""))
  yTest <- read.table(paste(currentDir,"/test/Y_test.txt",sep=""))
  subjectTrain <- read.table(paste(currentDir,"/train/subject_train.txt",sep=""))
  subjectTest <- read.table(paste(currentDir,"/test/subject_test.txt",sep=""))

  # Merges the training and the test sets to create one data set
  
  # create x data set
  xData <- rbind(xTrain, xTest)
  
  # create y data set
  yData <- rbind(yTrain, yTest)
  
  # create subject data set
  subjectData <- rbind(subjectTrain, subjectTest)
  
  #Extracts only the measurements on the mean and standard deviation for each measurement. 
  features <- read.table(paste(currentDir,"/features.txt",sep=""))
    
  # subset the desired columns
  xMeanAndStd <- xData[ grep(".*Mean.*|.*Std.*", features[,2])]
  
  #Uses descriptive activity names to name the activities in the data set
  
  activities <- read.table(paste(currentDir,"/activity_labels.txt",sep=""))

  yData[, 1] <- activities[yData[, 1], 2]
  
  names(yData) <- "activity"
  
  # Appropriately label the data set with descriptive variable names
  
  names(subjectData) <- "subject"
  
  allData <- cbind(xData, yData, subjectData)
  
  # From the data set in last step, creates a second, independent tidy data set 
  #with the average of each variable for each activity and each subject.
  
  averages <- ddply(allData, .(subject, activity), function(x) colMeans(x[, 1:66]))
  
  write.table(averages, "tidy.txt", row.name=FALSE)
}