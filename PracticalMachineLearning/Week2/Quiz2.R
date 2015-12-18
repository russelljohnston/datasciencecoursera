#Question 1
#Load the Alzheimer's disease data using the commands:

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

#Which of the following commands will create training and test sets with about 50% of the observations assigned to each?

#1:
# adData = data.frame(predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[-trainIndex,]

#2:
# adData = data.frame(diagnosis,predictors)
# trainIndex = createDataPartition(diagnosis, p = 0.50)
# training = adData[trainIndex,]
# testing = adData[-trainIndex,]

#3:
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


#4:
# adData = data.frame(diagnosis,predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[trainIndex,]



#Question 2
#Load the cement data using the commands:

library(AppliedPredictiveModeling)
library(caret)

data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#Make a histogram and confirm the SuperPlasticizer variable is skewed. 
#Normally you might use the log transform to try to make the data more symmetric. 
#Why would that be a poor choice for this variable?

hist(log(concrete$Superplasticizer+1))

# 1: The log transform is not a monotone transformation of the data.
# 2: The log transform produces negative values which can not be used by some classifiers.
 3: There are a large number of values that are the same and 
    even if you took the log(SuperPlasticizer + 1) they would still all be 
   identical so the distribution would not be symmetric.
# 4: The SuperPlasticizer data include negative values so the log transform can not be performed.
   
   
#Question 3
#Load the Alzheimer's disease data using the commands:
  
library(caret)
library(AppliedPredictiveModeling)
   
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
  
#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() 
#function from the caret package. Calculate the number of principal 
#components needed to capture 80% of the variance. 

df1 <- adData[ , grepl( "^IL" , names( adData ) ) ]
preProcess(df1, method="pca",thresh = 0.8, na.remove = TRUE)

dftrain <- training[ , grepl( "^IL" , names( training ) ) ]
preProcess(dftrain, method="pca",thresh = 0.8, na.remove = TRUE)

#How many are there?
 
#1.  11
#2.   9
#Answer
3.   7
#4.   8

#Question 4
#Load the Alzheimer's disease data using the commands:

library(caret)
library(AppliedPredictiveModeling)

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)




#Create a training data set consisting of only the predictors with 
#variable names beginning with IL and the diagnosis. Build two predictive models, 
#one using the predictors as they are and one using PCA with principal components 
#explaining 80% of the variance in the predictors. Use method="glm" in the train function. 

dataIL <- data.frame(adData$diagnosis, adData[ , grepl( "^IL" , names( adData )) ])
colnames(dataIL)[1]<-"diagnosis"

inTrain = createDataPartition(dataIL$diagnosis, p = 3/4)[[1]]
training = dataIL[ inTrain,]
testing = dataIL[-inTrain,]



modelFit1 <- train(diagnosis ~., data=training,preProcess=c("center","scale"),method="glm")
predictMF1=predict(modelFit1,newdata=testing)
acc1 <- confusionMatrix(testing$diagnosis,predictMF1)


preOBJ=preProcess(training[,-1],thresh=0.8,method="pca")
trainPCA=predict(preOBJ,training[,-1])
testPCA=predict(preOBJ,testing[,-1])

modelFit2=train(training$diagnosis~.,data=trainPC,method="glm")
predictMF2=predict(modelFit2,testPCA)
acc2 <- confusionMatrix(testing$diagnosis,predictMF2)
         


#What is the accuracy of each method in the test set? Which is more accurate?

Answer
1:
 Non-PCA Accuracy: 0.65 
 PCA Accuracy: 0.72
#2:
# Non-PCA Accuracy: 0.72 
# PCA Accuracy: 0.65
#3:
# Non-PCA Accuracy: 0.91 
# PCA Accuracy: 0.93
#4:
# Non-PCA Accuracy: 0.75 
# PCA Accuracy: 0.71

