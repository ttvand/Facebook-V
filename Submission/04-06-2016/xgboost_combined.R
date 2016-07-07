###############################
# Combined XGBOOST prediction #
###############################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Submission")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Target date
targetDate <- "23-05-2016"

# Submission date and file name
submissionDate <- "04-06-2016"
submissionFile <- "xgboostAverageBlend.csv"

# Processed test ids
testIds <- 1:27 # 1:287
nbTestIds <- length(testIds)

# Overwrite predictions file if it exists?
overwrite <- FALSE

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
predictionsPath <- file.path(getwd(), submissionDate, predictionsFolder)
dir.create(predictionsPath, showWarnings = FALSE)

# Add to submission at each iteration and try to load it from a file if 
# available
submission <- data.table(row_id = 0, place_id = "")
submission <- submission[0]

for(i in 1:nbTestIds){
  testId <- testIds[i]
  
  # Progress message
  cat("Combining file", i, "of", nbTestIds, "@",
      as.character(Sys.time()), "\n")
  
  # Load the predictions if they are available
  batchPredictionsFn <- paste("Random batch", testId, "predictions.rds")
  predictionsBatchPath <- file.path(predictionsPath, batchPredictionsFn)
  if(!overwrite && file.exists(predictionsBatchPath)){
    predictionsBatch <- readRDS(predictionsBatchPath)
  } else{
    
    # Load raw data file to match KNN with a row id
    rawPath <- paste0("../Downsampling/", targetDate, "/test",
                      " - Random batch - ", testId, ".rds")
    raw <- readRDS(rawPath)
    
    # Load the combined summary features file
    featuresDir <- paste0("../Feature engineering/", targetDate, "/",
                          "test")
    featuresPath <- file.path(featuresDir,
                              paste0("test features - Random batch - ",
                                     testId, ".rds"))
    
    # Load the validation combined features data and the model
    valFeatures <- readRDS(featuresPath)
    
    # Extract predictors data from the features data
    predictorData <- valFeatures[, combinedModels[[1]]$predictors, with=FALSE]
    
    # Convert the predictor data to a matrix
    predictorDataM <- as.matrix(predictorData)
    
    # Calculate the predicted place probabilities by summing the predicted
    # probabilities
    valFeatures[,predict := 0]
    for(j in 1:nbTopModels){
      valFeatures[,predict := predict +
                    predict(combinedModels[[j]]$model, predictorDataM)]
    }
    valFeatures[,order_predict := match(-predict, sort(-predict)), by=row_id]
    
    # Combine the top three neighbors to a string vector
    placeString <- paste(valFeatures[order_predict==1, top_place_id],
                         valFeatures[order_predict==2, top_place_id],
                         valFeatures[order_predict==3, top_place_id])
    
    if(length(placeString) != nrow(raw)) browser()
    
    # Save the row ids and place strings in a data table format
    predictionsBatch <- data.table(row_id = raw$row_id, place_id = placeString)
    saveRDS(predictionsBatch, file=predictionsBatchPath)
  }
  
  # Add the id and top 3 to the submission file
  submission <- rbind(submission, predictionsBatch)
}

# Extract template submission file
templateSubmissionPath <- paste0("../Data/sample_submission.rds")
paddedSubmission <- readRDS(templateSubmissionPath)

# Set the place id to an empty character string
paddedSubmission[,place_id := "1 2 3"]

# Replace the matched ids in padded submission by the combined submission file
paddedSubmission[submission$row_id + 1, place_id := submission$place_id]

# Write the padded submission to a csv file
write.csv(paddedSubmission, file.path(getwd(), submissionDate,
                                      submissionFile),
          row.names = FALSE)

# Display the successful submission message
cat("Submission file created successfully!\n",
    nrow(submission)," records were predicted (",
    round(nrow(submission)/nrow(paddedSubmission)*100,2),"%)\n", sep="")
