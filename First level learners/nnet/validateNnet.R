# NNet first level validation function

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners/nnet")

# Load the required libraries
library(data.table)
library(bit64)
library(nnet)

# Name of the xgboost model
trainModelExt <- "nnet - train 42 - size 20 - decay 0.3 - maxit 2000 - L20.rds"
validationIds <- 1
nbValidationIds <- length(validationIds)

# Source the prediction function
source("predictNnet.R")

# List the considered predictors and target variable
targetVar <- "placeMatch"

# Load validation batch from local system or external HD?
loadFromExternalHD <- TRUE


######################################################################

# Display the considered model string
cat(trainModelExt, "\n")

# Load the xgboost models
modelFolder <- "Models"

# Extract the train model
modelPath <- file.path(getwd(), modelFolder, trainModelExt)
trainModel <- readRDS(modelPath)

# Store the batch maps (Mean average prediction @3) in a vector
maps <- vector(mode = "numeric", length = nbValidationIds)

# Loop over all validation ids and return the according validation error
for(i in 1:nbValidationIds){
  validationId <- validationIds[i]
  
  # Feedback message
  cat("Calculating validation error for validation batch", validationId,
      "(nnet) \n")
  
  # Path to the combined features file of the validation batch
  if(loadFromExternalHD){
    featuresDir <- "H:/Facebook features"
  } else{
    featuresDir <- paste0("../Feature engineering/", targetDate, "/",
                          "train")
  }
  featuresPath <- file.path(featuresDir,
                            paste0("validation features - Random batch - ",
                                   validationId, ".rds"))
  
  # Load the validation combined features data and the model
  valFeatures <- readRDS(featuresPath)
  
  # Count the number of places that need to be predicted
  batchSize <- length(unique(valFeatures$row_id))
  nObs <- nrow(valFeatures)
  
  # Extract predictors data and labels from the features data
  predictorData <- valFeatures[, trainModel$predictors, with=FALSE]
  labels <- valFeatures[, targetVar, with=FALSE][[1]]
  
  # Convert the predictor data to a matrix
  predictorDataM <- as.matrix(predictorData)
  
  # Normalize the validation predictors
  predictorDataMS <- scale(predictorDataM,
                           center = trainModel$predictorMeans,
                           scale = trainModel$predictorSds)
  # predictorDataMS <-
  #   (predictorDataM - 
  #   matrix(rep(trainModel$predictorMeans, each=nObs), nrow = nObs))/
  #   matrix(rep(trainModel$predictorSds, each=nObs), nrow = nObs)
                           
  
  # Assess the validation accuracy (out-of-bag-error) by summing the predicted
  # probabilities
  valFeatures[,batchPredictions :=
                predict(trainModel$model, predictorDataMS)]
  
  valFeatures[,place_predict := place_id[which.max(batchPredictions)],
              by=row_id]
  valFeatures[,order_predict := match(1:length(batchPredictions),
                                      order(-batchPredictions)), by=row_id]
  
  # What is the order of the predicted ids?
  orderPredict <- valFeatures[placeMatch==TRUE, order_predict]
  
  mapBatch <- sum((orderPredict==1) + (orderPredict==2)/2 +
                    (orderPredict==3)/3)/batchSize
  maps[i] <- mapBatch
  
  # Base map for the validation batch
  baseMapBatch <- (valFeatures[topMRank==1, sum(placeMatch)] +
    valFeatures[topMRank==2, sum(placeMatch)]/2 +
    valFeatures[topMRank==3, sum(placeMatch)]/3) / batchSize
  
  # Display the validation measures
  cat("Base map @3 :",
      round(baseMapBatch*100,2),"\n")
  cat("Mean top class training %:",
      round(sum(orderPredict==1)/batchSize*100,2),"\n")
  cat("Mean average precision @3 :",
      round(mapBatch*100,2),"\n\n")
}

# Display the summary validation message
cat("Overall mean average precision @3 :", round(mean(maps)*100,2),"\n")