library(caret)
preprocessData <- function(df) {
  # remove columns with #DIV/0!
  df <- df[,-which(names(df) %in% c("new_window","amplitude_yaw_belt", "amplitude_yaw_forearm","amplitude_yaw_dumbbell", "kurtosis_yaw_forearm", "kurtosis_yaw_belt", "skewness_yaw_belt", "kurtosis_yaw_dumbbell", "skewness_yaw_dumbbell", "skewness_yaw_forearm"))]
  
  dropInd <- which(apply(df, 1, function(x) any(x=="#DIV/0!")))
  df <- df[-dropInd,]
  
  library(caret)
  
  # convert factors to numeric 
  facs <- sapply(df[,-ncol(df)], is.factor)
  df[ , facs] <- as.numeric(as.character(df[ , facs]))
  
  # set NA to 0
  df[is.na(df)] <- 0
  
  nsv <- nearZeroVar(df,saveMetrics=TRUE)
  
  # remove zero columns
  df <- df[,-which(names(df) %in% c(rownames(nsv[nsv$zeroVar,])))]
  return(df)
}


set.seed(2172015)
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")


training<-preprocessData(training)

testing[is.na(testing)] <- 0
testing <- testing[, which(names(testing) %in% names(training))]

fitControl <- trainControl(method="cv",
                           number=2,
                           repeats=1,
                           verboseIter=TRUE)

gbm_model<-train(classe~.,data=training,method="gbm",
                trControl=fitControl,
                preProcess=c("scale", "pca"),
                verbose=FALSE)
gbm_model
testGbm<- predict(gbm_model, testing)
testGbm

# preprocess, center, scale, pca
preProc <- preProcess(training[,-124], method="pca")
trainPC <- predict(preProc,training[,-124])

# RPart
library(rpart)
modelFit <- rpart(training$classe ~ .,method="class",data=trainPC)
modelFit
testPC <- predict(preProc,testing)
testRpart<- predict(modelFit, testPC, type="vector")
testRpart

# Random Forest

rf_model <- train(classe~.,data=training,method="rf",
                 trControl=fitControl,
                 preProcess=c("scale", "pca")) 
rf_model
testRf<- predict(rf_model, testing)
testRf