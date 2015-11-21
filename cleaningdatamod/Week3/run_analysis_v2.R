library(dplyr)

#Read data to data frame
test_xDF <-   read.table("UCI_HAR_Dataset/test/X_test.txt",header=FALSE,quote="")
test_yDF <-   read.table("UCI_HAR_Dataset/test/y_test.txt",header=FALSE,quote="")
sub_testDF <- read.table("UCI_HAR_Dataset/test/subject_test.txt",header=FALSE,quote="")

train_xDF <-   read.table("UCI_HAR_Dataset/train/X_train.txt",header=FALSE,quote="")
train_yDF <-   read.table("UCI_HAR_Dataset/train/y_train.txt",header=FALSE,quote="")
sub_trainDF <- read.table("UCI_HAR_Dataset/train/subject_train.txt",header=FALSE,quote="")

#Add header 
labels <- read.table("UCI_HAR_Dataset/features.txt",header=FALSE)
labels.T <- t(labels[c(2)])
names(test_xDF) <- labels.T
names(test_yDF) <- c("y_test")
names(sub_testDF) <- c("subject_test")
names(train_xDF) <- labels.T
names(train_yDF) <- c("y_test")
names(sub_trainDF) <- c("subject_test")

#Merging  DATA SETS
testJoin <- cbind(test_xDF,test_yDF,sub_testDF)
trainJoin <- cbind(train_xDF,train_yDF,sub_trainDF)
MergedData <- rbind(testJoin, trainJoin)


MeanStd.df <- MergedData[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)]
# Averages <- colMeans(MeanStd.df)
Averages <- sapply(MeanStd.df, mean)

write.table(Averages, "averages.txt",row.name=FALSE)
# extractS<-MergedData[,grep("mean()-", colnames(MergedData)) | grepl("^std()$", colnames(MergedData))]



