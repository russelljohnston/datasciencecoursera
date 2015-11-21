library(dplyr)

#Read data to data frame

labels <- read.table("UCI_HAR_Dataset/features.txt",header=FALSE)
labels.T <- t(labels[c(2)])


test_xDF <-   read.table("UCI_HAR_Dataset/test/X_test.txt",header=FALSE,quote="")
names(test_xDF) <- labels.T
test_yDF <-   read.table("UCI_HAR_Dataset/test/y_test.txt",header=FALSE,quote="")
names(test_yDF) <- c("y_test")
sub_testDF <- read.table("UCI_HAR_Dataset/test/subject_test.txt",header=FALSE,quote="")
names(sub_testDF) <- c("subject_test")

train_xDF <-   read.table("UCI_HAR_Dataset/train/X_train.txt",header=FALSE,quote="")
names(train_xDF) <- labels.T
train_yDF <-   read.table("UCI_HAR_Dataset/train/y_train.txt",header=FALSE,quote="")
names(train_yDF) <- c("y_train")
sub_trainDF <- read.table("UCI_HAR_Dataset/train/subject_train.txt",header=FALSE,quote="")
names(sub_trainDF) <- c("subject_train")


#JOINING TEST DATA SETS
testJoin <- cbind(test_xDF,test_yDF,sub_testDF)
#adding extra rows of NA to match number of rows of training dataset
na.count <- nrow(train_xDF) - nrow(testJoin)
na.dm <- matrix(NA, na.count, ncol(testJoin))
colnames(na.dm) <- colnames(testJoin)
testJoin <- rbind(testJoin, na.dm)
# testJoin$ID<-seq.int(nrow(testJoin)) # create column of index

#JOINING TRAIN DATA SETS
trainJoin <- cbind(train_xDF,train_yDF,sub_trainDF)
##JOINING TRAIN-TEST DATA SETS
JoinedData <- cbind(trainJoin,testJoin)


extract <- JoinedData[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543)]

extractS<-testJoin[,grepl("mean()$", colnames(testJoin)) | grepl("^std()$", colnames(testJoin))]



