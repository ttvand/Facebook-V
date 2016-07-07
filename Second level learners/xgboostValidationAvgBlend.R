# XGBOOST second level validation

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Second level learners")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Train range: 131:150
# Validation range: 21:30; 1:4
trainIds <- 131:160
hyperTrainExt <- ""
nbTrainIds <- length(trainIds)
validationIds <- 21:30
nbValidationIds <- length(validationIds)

# External path to load predictions from
loadFromExternalHD <- FALSE
externalPredictionsFolder <- "H:/First level combined predictions"

# Option to order the predictions based on the average rank rather than the
# average prediction score. Ties are resolved by considering the average
# prediction score
averageMethod <- c("prediction", "rank")[1]

# List the considered predictors and target variable
targetVar <- "placeMatch"


######################################################################

# Load the xgboost models
modelFolder <- file.path(getwd(), "xgboost models")

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
  
  # Path to the combined predictions file of the validation batch
  if(loadFromExternalHD){
    featuresDir <- externalPredictionsFolder
  } else{
    featuresDir <-
      file.path(paste0("../First level learners/Combined predictions"))
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
  
  # Calculate the row means of the model columns
  predMeans <- rowSums(predictorDataM[,c(rep(2,15), 3:ncol(predictorDataM))])/
    (ncol(predictorDataM) + 13)
  
  # Assess the training accuracy (out-of-bag-error) by summing the predicted
  # probabilities
  valFeatures[,predict := 0]
  valFeatures[,rankSum := 0]
  valFeatures[,predMeans := predMeans]
  for(j in 1:nbTrainIds){
    valFeatures[,batchPredictions :=
                  predict(combinedModels[[j]]$model, predictorDataM,
                          missing = NA)]
    gc()
    valFeatures[,predict := predict + batchPredictions]
    if(averageMethod == "rank"){
      valFeatures[,batch_order := match(1:length(batchPredictions),
                                        order(-batchPredictions)), by=row_id]
      valFeatures[,rankSum := rankSum + batch_order]
    }
  }
  
  if(averageMethod == "prediction"){
    valFeatures[,place_predict := place_id[which.max(predict)], by=row_id]
    valFeatures[,order_predict := match(1:length(predict),
                                        order(-predict)), by=row_id]
  } else{
    valFeatures[,place_predict := place_id[which.min(rankSum - predict*1e-6)],
                by=row_id]
    valFeatures[,order_predict := match(1:length(predict),
                                        order(rankSum - predict*1e-6)),
                by=row_id]
  }
  
  valFeatures[,order_predict_pm := match(1:length(predMeans),
                                         order(-predMeans)), by=row_id]
  
  # What is the order of the predicted ids?
  orderPredict <- valFeatures[placeMatch==TRUE, order_predict]
  table(orderPredict)
  
  # What is the order of the mean base model predictions?
  orderPredictPm <- valFeatures[placeMatch==TRUE, order_predict_pm]
  table(orderPredictPm)
  
  mapBatch <- sum((orderPredict==1) + (orderPredict==2)/2 +
                    (orderPredict==3)/3)/batchSize
  maps[i] <- mapBatch
  
  # Check for ties in the ordering (should not occur)
  if(sum(valFeatures$topMRank==3) != batchSize) browser()
  
  # Base mean top class score 
  baseTopClass <- valFeatures[topMRank==1, sum(placeMatch)] / batchSize
  
  # Base map for the validation batch
  baseMapBatch <- (valFeatures[topMRank==1, sum(placeMatch)] +
                     valFeatures[topMRank==2, sum(placeMatch)]/2 +
                     valFeatures[topMRank==3, sum(placeMatch)]/3) / batchSize
  
  # Display the training accuracy
  cat("Base top class % :",
      round(baseTopClass*100,2),"\n")
  cat("Base map @3 :",
      round(baseMapBatch*100,2),"\n")
  cat("Mean top class %:",
      round(sum(orderPredict==1)/batchSize*100,2),"\n")
  cat("Mean average precision @3 :",
      round(mapBatch*100,2),"\n\n")
}

# Display the summary validation message
cat("Overall mean average precision @3 :", round(mean(maps)*100,2),"\n")