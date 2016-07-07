# XGBOOST first level validation

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners/Simple xgboost blend")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Target date
targetDate <- "23-05-2016"

# Train range: 26:45
# Validation range: 1:10
trainIds <- 26:45 #26:45
# hyperTrainExt <- " - 200 - 9 - 0.6 - 11"
hyperTrainExt <- ""
nbTrainIds <- length(trainIds)
validationIds <- 1:4 #1:4
nbValidationIds <- length(validationIds)

# External path to load features from
loadFromExternalHD <- TRUE
externalFeaturesFolder <- "H:/Facebook features"

# Option to order the predictions based on the average rank rather than the
# average prediction score. Ties are resolved by considering the average
# prediction score
averageMethod <- c("prediction", "rank")[1]

# List the considered predictors and target variable
targetVar <- "placeMatch"


######################################################################

# Load the xgboost models
modelFolder <- file.path(getwd(), targetDate)

# Extract the models
combinedModels <- vector(mode = "list", length = nbTrainIds)
for(i in 1:nbTrainIds){
  # Load the model
  trainId <- trainIds[i]
  fn <- paste0("xgboost - Random batch - ", trainId, hyperTrainExt, ".rds")
  combinedModels[[i]] <- readRDS(file.path(modelFolder, fn))
}

# Store the batch maps (Mean average prediction @3) in a vector
maps <- vector(mode = "numeric", length = nbValidationIds)

# Loop over all validation ids and return the according validation error
for(i in 1:nbValidationIds){
  validationId <- validationIds[i]
  
  # Feedback message
  cat("Calculating validation error for validation batch", validationId, "\n")
  
  # Path to the combined features file of the validation batch
  if(loadFromExternalHD){
    featuresDir <- externalFeaturesFolder
  } else{
    featuresDir <- paste0("../../Feature engineering/", targetDate, "/",
                          "train")
  }
  featuresPath <- file.path(featuresDir,
                            paste0("validation features - Random batch - ",
                                   validationId, ".rds"))
  
  # Load the validation combined features data and the model
  valFeatures <- readRDS(featuresPath)
  
  
  # Count the number of places that need to be predicted
  batchSize <- length(unique(valFeatures$row_id))
  
  # Extract predictors data and labels from the features data
  predictorData <- valFeatures[, combinedModels[[1]]$predictors, with=FALSE]
  labels <- valFeatures[, targetVar, with=FALSE][[1]]
  
  # Convert the predictor data to a matrix
  predictorDataM <- as.matrix(predictorData)
  
  # Assess the training accuracy (out-of-bag-error) by summing the predicted
  # probabilities
  valFeatures[,predict := 0]
  valFeatures[,rankSum := 0]
  for(j in 1:nbTrainIds){
    valFeatures[,batchPredictions :=
                  predict(combinedModels[[j]]$model, predictorDataM,
                          missing = NA)]
    gc()
    valFeatures[,predict := predict + batchPredictions]
    if(averageMethod == "rank"){
      valFeatures[,batch_order := match(1:length(batchPredictions),
                                        order(-batchPredictions)), by=.(x,y)]
      valFeatures[,rankSum := rankSum + batch_order]
    }
  }
  
  if(averageMethod == "prediction"){
    valFeatures[,place_predict := place_id[which.max(predict)], by=.(x,y)]
    valFeatures[,order_predict := match(1:length(predict),
                                        order(-predict)), by=.(x,y)]
  } else{
    valFeatures[,place_predict := place_id[which.min(rankSum - predict*1e-6)],
                by=.(x,y)]
    valFeatures[,order_predict := match(1:length(predict),
                                        order(rankSum - predict*1e-6)),
                by=.(x,y)]
  }
  
  # What is the order of the predicted ids?
  orderPredict <- valFeatures[placeMatch==TRUE, order_predict]
  table(orderPredict)
  
  mapBatch <- sum((orderPredict==1) + (orderPredict==2)/2 +
                    (orderPredict==3)/3)/batchSize
  maps[i] <- mapBatch
  
  # Display the training accuracy
  cat("Mean top class training %:",
      round(sum(orderPredict==1)/batchSize*100,2),"\n")
  cat("Mean average precision @3 :",
      round(mapBatch*100,2),"\n\n")
}

# Display the summary validation message
cat("Overall mean average precision @3 :", round(mean(maps)*100,2),"\n")