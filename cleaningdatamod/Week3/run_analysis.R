library(dplyr)
library(tidyr)

#Reading from dir data/ into dataframes
test_xDF <-   read.table("data/X_test.txt",header=FALSE,quote="")
test_yDF <-   read.table("data/y_test.txt",header=FALSE,quote="")
sub_testDF <- read.table("data/subject_test.txt",header=FALSE,quote="")

activity_labels <- read.table("data/activity_labels.txt",header=FALSE)
names(activity_labels) <- c("activity","label")

train_xDF <-   read.table("data/X_train.txt",header=FALSE,quote="")
train_yDF <-   read.table("data/y_train.txt",header=FALSE,quote="")
sub_trainDF <- read.table("data/subject_train.txt",header=FALSE,quote="")

#Merging  DATA SETS
testJoin <- cbind(test_yDF,sub_testDF,test_xDF)
trainJoin <- cbind(train_yDF,sub_trainDF,train_xDF)
MergedData <- rbind(testJoin, trainJoin)

#Adding header 
labels <- read.table("data/features.txt",header=FALSE)
labels.T <- t(labels[c(2)])
names(test_xDF) <- labels.T
names(MergedData) <- c("activity","subject",labels.T)

#Merge table with the activity text labels:
MergedData <- merge(MergedData, activity_labels, by = "activity") 

#Selecting mean and standard deviation based on following text pattern:
MeanStd <- MergedData[,grepl("subject", colnames(MergedData)) 
                         | grepl("label", colnames(MergedData)) 
                         | grepl("mean\\(", colnames(MergedData)) 
                         | grepl("std\\(", colnames(MergedData))]



#Tidying the table header labels a little. The codebook.txt should provide 
#more details regarding interprating the labeling convention:
names(MeanStd) <- tolower(names(MeanStd))
names(MeanStd) <-  names(MeanStd) %>%
  gsub("\\()", "", .) %>%
  gsub("-", "", .) %>%
  gsub("std", "standarddeviation", .) %>%
  gsub("tb", "timeb", .) %>%
  gsub("tg", "timeg", .) %>%
  gsub("fb", "frequencyb", .) %>%
  gsub("fg", "frequencyg", .) %>%
  gsub("gyro", "gyroscope", .) %>%
  gsub("bodybody", "body", .) %>%
  gsub("acc", "accelerometer", .)

#Now computing the mean grouped by subject and activity (label):
tidyDF <- tbl_df(MeanStd) %>%
  gather(measurement, value, -subject, -label) %>%
  group_by(subject, label, measurement) %>% 
  summarise(mean=mean(value))

#Finally, write this new tidy data set to a file.
write.table(tidyDF, "tidyData.txt",sep="\t", quote=FALSE, row.name=FALSE)


