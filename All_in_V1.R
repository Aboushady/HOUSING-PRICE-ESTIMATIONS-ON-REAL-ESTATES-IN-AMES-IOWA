outStr  <- "/media/verbonus/Work/Uppsala/Uppsala University/Semester 2/Period 3/Machine Learning/Project/"
dataSet <- read.csv(paste(outStr, "train.csv", sep = ""), header = TRUE, sep = ",")

#Pre-processing [begin]

#deleting some columns.
drops <- c("Street", "Alley", "LandContour", "Utilities", "LandSlope", 
           "Condition2", "Bldgtype", "RoofMatl", "ExterQual", "BsmtExposure", "BsmtFinType2",
           "BsmtFinSF2", "Heating", "GarageCars", "GarageCond", "PoolQC")

dataSet <- dataSet[ , !(names(dataSet) %in% drops)]

#Saving to a new csv file
write.table(dataSet, file=paste(outStr, "New_train.csv", sep=""),
           sep=",", row.names = F)



#Pre-processing [End]

#Reading the new data set to the same variable "dataSet",
dataSet <- read.csv(paste(outStr, "New_train.csv", sep = ""), header = TRUE, sep = ",")


#Converting categorical attributes to binary using One-Hot-Encoding.[begin]

#Getting all categorical attributes names
factorAttribute   <- unlist(lapply(dataSet, is.factor))
df_subset         <- names(dataSet[ , factorAttribute]) 

#Applying One-hot-encoding
library(dummies)
encodedData <- dummy.data.frame(dataSet, names=df_subset)

#Converting categorical attributes to binary using One-Hot-Encoding.[End]

encodedData[is.na(encodedData)] <- 0

#Rescaling 0 values.[Begin]

numericAtt       <- unlist(lapply(dataSet, is.numeric))
df_NumericSet    <- names(encodedData[ , numericAtt]) 

remove <- c("MSZoningC (all)", "Exterior2ndWd Sdng", "Exterior2ndWd Shng", "Exterior1stWd Sdng", "Exterior2ndBrk Cmn")
df_NumericSet <- df_NumericSet[! df_NumericSet %in% remove]

for (k in df_NumericSet)
{
  temp <- eval(parse(text=paste("encodedData$", k , sep="")))
  temp <- ifelse(temp == 0, median(temp), temp)
}

encodedData$`MSZoningC (all)`    <- ifelse(encodedData$`MSZoningC (all)`==0, median(encodedData$`MSZoningC (all)`), encodedData$`MSZoningC (all)`)
encodedData$`Exterior1stWd Sdng` <- ifelse(encodedData$`Exterior1stWd Sdng`==0, median(encodedData$`Exterior1stWd Sdng`), encodedData$`Exterior1stWd Sdng`)
encodedData$`Exterior2ndBrk Cmn` <- ifelse(encodedData$`Exterior2ndBrk Cmn`==0, median(encodedData$`Exterior2ndBrk Cmn`), encodedData$`Exterior2ndBrk Cmn`)
encodedData$`Exterior2ndWd Sdng` <- ifelse(encodedData$`Exterior2ndWd Sdng`==0, median(encodedData$`Exterior2ndWd Sdng`), encodedData$`Exterior2ndWd Sdng`)
encodedData$`Exterior2ndWd Shng` <- ifelse(encodedData$`Exterior2ndWd Shng`==0, median(encodedData$`Exterior2ndWd Shng`), encodedData$`Exterior2ndWd Shng`)

#Removing SalePrice attribute before scaling the data, then adding it back to it.
encodedDataLabels     <- encodedData[, 242]
encodedData           <- scale(encodedData[, 2:241])

encodedData <- cbind(encodedData, encodedDataLabels)

#Rescaling 0 values.[End]

#Splitting the data into K-folds && Applying PCA[Begin]

encodedData<-encodedData[sample(nrow(encodedData)),]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(encodedData)),breaks=5,labels=FALSE)

#Perform 5 folds cross validation
for(i in 1:5)
{
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData    <- encodedData[testIndexes, ]
  trainData   <- encodedData[-testIndexes, ]
  
  
  #Remove SalePrice attribute.
  testDataLables <- testData[, 241]
  testData       <- testData [, 1:240]
  trainDataLabels<- trainData[, 241]
  trainData      <- trainData[, 1:240]
  
  
  #Apply PCA on training set. [begin]
  prin_comp_ana <- prcomp(trainData, scale. = )
  
  #add a training set with principal components
  trainData$flag <- 1
  trainData      <- data.frame(flag = trainData$flag, prin_comp_ana$x)
  trainData      <- trainData[,-1]
  #Apply PCA on training set. [End]
   
  
  #Apply PCA on test set. [Begin]
  colnames(testData) <- rownames(prin_comp_ana$rotation)
  testData           <- predict(prin_comp_ana, newdata = testData)
  testData           <- as.data.frame(testData)
  #Apply PCA on test set. [End]
  
  
  #Adding SalePrice Attribute.
  trainData      <- cbind(trainData, trainDatalabels)
  testData       <- cbind(testData, testDataLables)
  
  #Save every train and test data to a new file.
  outTrainFile     <- paste("train", i, sep="_")
  outTrainStrPath  <- paste(outStr, outTrainFile, sep = "")
  write.csv(trainData, file=paste(outTrainStrPath, ".csv", sep = ""),
              sep=",", row.names = FALSE)
  outTestFile     <- paste("test", i, sep = "_")
  outTestStrPath  <- paste(outStr, outTestFile, sep = "")
  write.csv(testData, file=paste(outTestStrPath, ".csv", sep = ""),
              sep=",", row.names = FALSE)
}

#Splitting the data into K-folds[End]

#Creating all PCAs files Training and Testing [Begin]
for (l in 1:5)
{
  #Test set.
  outTestFile <- paste("test", l, sep = "_")
  outTestStrPath  <- paste(outStr, outTestFile, sep = "")
  tempTest <- read.csv(paste(outTestStrPath, ".csv", sep=""),
                header=TRUE, sep=",")
  tempTestLabels <- tempTest[, 241]
  tempTest <- tempTest[, 1:10]
  tempTest <- cbind(tempTest, tempTestLabels)
  
  outTestFile    <- paste("test_PCA_10", l, sep = "_")
  outTestStrPath    <- paste(outStr, outTestFile, sep = "")
  write.table(tempTest, file=paste(outTestStrPath, ".csv", sep = ""),sep = ",", row.names = FALSE)
  
  #Train set.
  outTrainFile     <- paste("train", l, sep="_")
  outTrainStrPath  <- paste(outStr, outTrainFile, sep = "")
  tempTrain        <- read.csv(paste(outTrainStrPath, ".csv", sep=""),
                              header=TRUE, sep=",")
  tempTrainLabels  <- tempTrain[, 241]
  tempTrain        <- tempTrain[,  1:10]
  tempTrain        <- cbind(tempTrain, tempTrainLabels)
  
  outTrainFile     <- paste("train_PCA_10", l, sep="_")
  outTrainStrPath  <- paste(outStr, outTrainFile, sep = "")
  write.table(tempTrain, file=paste(outTrainStrPath, ".csv", sep = ""),sep = ",", row.names = FALSE)
}

#Creating all PCAs files Training and Testing [End]

#KNN TEST [Begin]

trainData  <- read.csv(paste(outStr, "train_PCA_150_1.csv", sep = ""), header=TRUE,sep = ",")
testData   <- read.csv(paste(outStr, "test_PCA_150_1.csv", sep = ""), header=TRUE,sep = ",")

trainValues <- trainData[, 151]
trainLabels <- ifelse(trainValues < 254950, 0, ifelse(trainValues > 454950, 2, 1) )  
trainData <- trainData[, 1:150]

testValues <- testData[, 151]
testLabels <- ifelse(testValues < 254950, 0, ifelse(testValues > 454950, 2, 1) )  
testData <- testData[, 1:150]



library(class)
prc_test_pred <- knn(train = trainData, test = testData,cl = trainLabels, k=18)

#Calculating Error
train.error <- sum(prc_test_pred != trainLabels) / length(trainLabels)

library(gmodels)
CrossTable(x=testLables, y=prc_test_pred, prop.chisq = FALSE)

tb <- table(testLabels, prc_test_pred)

library(caret)
cm <- confusionMatrix(tb)

#KNN [End]

#K-folds with KNN Classification[Begin]
train.error <- character()
train.error.v20<- character() 
for(i in 1:5)
{
  for(j in 1:4)
  {
    pcaNum = switch(j,
               "1" = "10",
               "2" = "50",
               "3" = "100",
               "4" = "150")
    
    outTrainPCA <- paste("train_PCA", pcaNum, sep = "_")
    outTrainFile<- paste(outTrainPCA, i, sep = "_")
    outTrainStrPath  <- paste(outStr, outTrainFile, sep = "")
    tempTrain <- read.csv(paste(outTrainStrPath, ".csv", sep=""),
                         header=TRUE, sep=",")
    
    outTestPCA    <- paste("test_PCA", pcaNum, sep = "_")
    outTestFile   <- paste(outTestPCA, i, sep = "_")
    outTestStrPath    <- paste(outStr, outTestFile, sep = "")
    tempTest <- read.csv(paste(outTestStrPath, ".csv", sep=""),
                          header=TRUE, sep=",")
    
    trainValues <- tempTrain[, dim(tempTrain)[2]]
    #trainLabels <- ifelse(trainValues < 254950, 0, ifelse(trainValues > 454950, 2, 1) )  
    tempTrain <- tempTrain[, 1:dim(tempTrain)[2]-1]
    
    testValues <- tempTest[, dim(tempTest)[2]]
    #testLabels <- ifelse(testValues < 254950, 0, ifelse(testValues > 454950, 2, 1) )  
    tempTest <- tempTest[, 1:dim(tempTest)[2]-1]
    
    
    
    #library(class)
    #prc_test_pred <- knn(train = tempTrain, test = tempTest,cl = trainLabels, k=15)
    
    #train.error.v20 <- c(train.error.v20, sum(prc_test_pred != trainLabels) / length(trainLabels))
    knn_reg_pred <- FNN::knn.reg(train = tempTrain, test = tempTest, y = trainValues, k = 8)
    error <- testValues - knn_reg_pred$pred
    train.error <- c(train.error, sqrt(mean(error^2)))
  }
}
tb <- table(testLabels, prc_test_pred)
cm_v1 <- confusionMatrix(tb)
#K-folds with KNN Classification[End]

#KNN Regression [Begin]
trainData  <- read.csv(paste(outStr, "train_5.csv", sep = ""), header=TRUE,sep = ",")
testData   <- read.csv(paste(outStr, "test_5.csv", sep = ""), header=TRUE,sep = ",")

trainValues <- trainData[, 241]
#trainLabels <- ifelse(trainValues < 254950, 0, ifelse(trainValues > 454950, 2, 1) )  
trainData <- trainData[, 1:240]

testValues <- testData[, 241]
#testLabels <- ifelse(testValues < 254950, 0, ifelse(testValues > 454950, 2, 1) )  
testData <- testData[, 1:240]



knn_reg_pred <- FNN::knn.reg(train = trainData, test = testData, y = trainValues, k = 8)

error_All_Data <- testValues - knn_reg_pred$pred
train.error.All <- sqrt(mean(error_All_Data^2))

testData <- cbind(testData, testValues)
predicted <- predict(knn_reg_pred$pred, testData[, 152]) 

testValuesLess <- testValues - 10000
testValuesMore <- testValues + 10000

endPointsLess <- data.frame(testData$PC2, testValuesLess)
endPointsMore <- data.frame(testData$PC2, testValuesMore)


ggplot(testData, aes(x = testData$PC2, y=testValues)) +
  #geom_segment(aes(xend = knn_reg_pred$pred, yend = testData$PC2)) +
  geom_point() +
  geom_point(aes(y = knn_reg_pred$pred), shape = 1, color="brown") +
  geom_line(data=endPointsLess,  stat = "smooth", method = "loess", aes(x = testData$PC2, y= mean(testValuesLess), color=testValuesLess), color="brown", size=1)+
  geom_line(data=endPointsMore,  stat = "smooth", method = "loess", aes(x = testData$PC2 , y = mean(testValuesMore) , color=testValuesMore), color="yellow", size=1)+
  geom_line(data=testData, stat = "smooth", method = "loess", aes(x = testData$PC2, y=mean(knn_reg_pred$pred)), color="green", size=1)+
  scale_y_continuous(name="Responce Variable") +
  scale_x_continuous(name = "Predictor Variable" )
#KNN Regression [End]