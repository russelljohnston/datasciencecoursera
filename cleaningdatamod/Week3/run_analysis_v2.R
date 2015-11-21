library(dplyr)

#Read data to data frame
test_xDF <-   read.table("data/X_test.txt",header=FALSE,quote="")
test_yDF <-   read.table("data/y_test.txt",header=FALSE,quote="")
sub_testDF <- read.table("data/subject_test.txt",header=FALSE,quote="")

train_xDF <-   read.table("data/X_train.txt",header=FALSE,quote="")
train_yDF <-   read.table("data/y_train.txt",header=FALSE,quote="")
sub_trainDF <- read.table("data/subject_train.txt",header=FALSE,quote="")

#Add header 
labels <- read.table("data/features.txt",header=FALSE)
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
#compute averages across columns and transpose data into row format
Averages <- colMeans(MeanStd.df)

duplicated(names(Averages))
#Tidying the header
#force lowercase on labels
names(Averages) <- tolower(names(Averages))
#Remove () characters
names(Averages) <- gsub("\\()", "", names(Averages))

write.table(t(Averages), "averages.txt",row.name=FALSE)
# extractS<-MergedData[,grep("mean()-", colnames(MergedData)) | grepl("^std()$", colnames(MergedData))]



