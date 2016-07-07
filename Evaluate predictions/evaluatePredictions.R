#################################
# Prediction evaluation logic #
#################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Evaluate predictions")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(ggplot2)
library(viridis)

# Target date
targetDate <- "23-05-2016"

# Processed validation ids
validationIds <- 1:8 # 1:287
nbValidationIds <- length(validationIds)

# Overwrite predictions file if it exists?
overwrite <- TRUE

# Prediction subfolder
predictionsFolder <- "Predictions"


######################################################################

# Load the xgboost models which are being evaluated
modelFolder <- file.path("..", "First level learners", targetDate,
                         "Test prediction")
topModelNames <- list.files(modelFolder)
nbTopModels <- length(topModelNames)

# Extract the models
combinedModels <- vector(mode = "list", length = nbTopModels)
for(i in 1:nbTopModels){
  # Load the model
  combinedModels[[i]] <- readRDS(file.path(modelFolder, topModelNames[i]))
}

# Create predictions subfolder
# Create the target folder if it does not exist yet
predictionsPath <- file.path(getwd(), predictionsFolder)
dir.create(predictionsPath, showWarnings = FALSE)

# Add to submission at each iteration and try to load it from a file if 
# available
predictions <- data.table(row_id = 0, x = 0, y = 0, time = 0, accuracy = 0,
                          actual_place = "", predict_place = "", place_id = "")
predictions <- predictions[0]

for(i in 1:nbValidationIds){
  validationId <- validationIds[i]
  
  # Progress message
  cat("Combining file", i, "of", nbValidationIds, "@",
      as.character(Sys.time()), "\n")
  
  # Load the predictions if they are available
  batchPredictionsFn <- paste("Random batch", validationId, "predictions.rds")
  predictionsBatchPath <- file.path(predictionsPath, batchPredictionsFn)
  if(!overwrite && file.exists(predictionsBatchPath)){
    predictionsBatch <- readRDS(predictionsBatchPath)
  } else{
    
    # Load raw data file to match KNN with a row id
    rawPath <- paste0("../Downsampling/", targetDate, "/validation",
                      " - Random batch - ", validationId, ".rds")
    raw <- readRDS(rawPath)
    
    # Load the combined summary features file
    featuresDir <- paste0("../Feature engineering/", targetDate, "/",
                          "train")
    featuresPath <- file.path(featuresDir,
                              paste0("validation features - Random batch - ",
                                     validationId, ".rds"))
    
    # Load the validation combined features data and the model
    valFeatures <- readRDS(featuresPath)
    
    # Extract predictors data from the features data
    predictorData <- valFeatures[, combinedModels[[1]]$predictors, with=FALSE]
    
    # Convert the predictor data to a matrix
    predictorDataM <- as.matrix(predictorData)
    
    # Calculate the predicted place probabilities by summing the predicted
    # probabilities
    valFeatures[,predict := rnorm(nrow(valFeatures))*1e-10]
    for(j in 1:nbTopModels){
      valFeatures[,predict := predict +
                    predict(combinedModels[[j]]$model, predictorDataM,
                            missing = NA)]
    }
    valFeatures[,order_predict := match(-predict, sort(-predict)), by=row_id]
    
    # Combine the top three neighbors to a string vector
    placeString <- paste(valFeatures[order_predict==1, top_place_id],
                         valFeatures[order_predict==2, top_place_id],
                         valFeatures[order_predict==3, top_place_id])
    
    # Save the matched input data and predictions in a data table format
    matchIds <- match(unique(valFeatures$row_id), raw$row_id)
    predictionsBatch <- data.table(row_id = raw$row_id[matchIds],
                                   x = raw$x[matchIds], y = raw$y[matchIds],
                                   time = raw$time[matchIds],
                                   accuracy = raw$accuracy[matchIds],
                                   actual_place = as.character(
                                     raw$place_id[matchIds]),
                                   predict_place = as.character(
                                     valFeatures[order_predict==1,
                                                 top_place_id]),
                                   place_id = placeString)
    saveRDS(predictionsBatch, file=predictionsBatchPath)
  }
  
  # Add the id and top 3 to the submission file
  predictions <- rbind(predictions, predictionsBatch)
}


##################################
# Analyse validation predictions #
##################################

# Add a variable to indicate the prediction score (MAP@3)
predictions[,secondPrediction := unlist(strsplit(place_id," "))[2], by=row_id]
predictions[,thirdPrediction := unlist(strsplit(place_id," "))[3], by=row_id]
predictions[,predictionCorrect := as.numeric(actual_place == predict_place)]
predictions[,predictionScore := ifelse(actual_place == predict_place, 1,
                                       ifelse(actual_place == secondPrediction,
                                              1/2,
                                              as.numeric(actual_place ==
                                                           thirdPrediction)/
                                                3))]

# Make sure that the blocked values are not at the extremes
predictions[x>9.95, x:=9.95]
predictions[y>9.95, y:=9.95]

# Study the prediction performance by x-y block
p <- ggplot(predictions, aes(x, y, z = predictionScore)) +
  stat_summary_2d(fun = mean, bins = 25) +
  scale_fill_viridis()

ggplot(predictions, aes(x, y, z = predictionCorrect)) +
  stat_summary_2d(fun = mean, bins = 25) +
  scale_fill_viridis()

# Study the prediction performance by time
predictions[,timePeriod := cut_number(time,25)]
meanTimeGroupPredictions <- predictions[,.(meanPredScore =
                                             mean(predictionScore)),
                                        by=timePeriod]
q <- ggplot(meanTimeGroupPredictions, aes(x=timePeriod, y=meanPredScore)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
print(q)

# Study the prediction performance by accuracy
predictions[,accuracyGroup := cut_number(accuracy,25)]
meanAccuracyGroupPredictions <- predictions[,.(meanPredScore =
                                             mean(predictionScore)),
                                        by=accuracyGroup]
r <- ggplot(meanAccuracyGroupPredictions, aes(x=accuracyGroup, y=meanPredScore)) + 
  geom_point() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# Interesting effect at high X range
print(p)

# Predictions harder further in prediction period -> this is a GOOD
# sign since it indicates that more recent time features are leveraged
print(q)

# Accuracy group plays an important role
print(r)

