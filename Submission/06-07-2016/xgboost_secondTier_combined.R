###########################################
# Combined second tier XGBOOST prediction #
###########################################

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Submission")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Submission date and file name
submissionDate <- "06-07-2016"
submissionFile <- "xgboostSecondTierAverageBlend - Batch 1-287 - 121 features - topMPredictFactor 3.csv"

# Processed test ids
testIds <- 1:287 # 1:287
nbTestIds <- length(testIds)

# Relative importance of the top M prediction (set to 1e-6 instead of zero to
# handle ties)
topMPredictFactor <- 3

# Execution time range in 24 hour notation (option to start at certain hour)
timeRange <- c(0,24)

# Target date
targetDate <- "23-05-2016"

# Option to load the joined features file from an external HD.
loadFromExternalHD <- TRUE
externalHDPath <- "H:/First level combined predictions"

# Overwrite predictions files if they exist?
overwrite <- FALSE

# Prediction subfolder
predictionsFolder <- "Predictions"


######################################################################

# Load the xgboost models which are being evaluated
modelFolder <- file.path("..", "Second level learners", "xgboost models",
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
    if(loadFromExternalHD){
      featuresDir <- externalHDPath
    } else{
      featuresDir <- paste0("../First level learners/Combined predictions")
    }
    featuresPath <- file.path(featuresDir,
                              paste0("test features - Random batch - ",
                                     testId, ".rds"))
    
    # Wait until the features file is available
    while(TRUE){
      exit <- file.exists(featuresPath) &&
        as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
        as.numeric(format(Sys.time(),"%H")) <= timeRange[2]
      
      if(exit){
        successfulExit <- FALSE
        try({
          # Load the test combined features data
          testFeatures <- readRDS(featuresPath)
          
          successfulExit <- TRUE
        }, silent=TRUE)
        if(successfulExit){
          break
        }
      }
      
      # Display sleep message
      cat(paste0("Test features not available, sleeping for five minutes @"),
          as.character(Sys.time()), "\n")
      
      # Sleep since the features file is not available
      Sys.sleep(300) 
    }
    
    # Extract predictors data from the features data
    predictorData <- testFeatures[, combinedModels[[1]]$predictors, with=FALSE]
    
    # Convert the predictor data to a matrix
    predictorDataM <- as.matrix(predictorData)
    
    # Calculate the predicted place probabilities by summing the predicted
    # probabilities
    testFeatures[,predict := 0]
    for(j in 1:nbTopModels){
      testFeatures[,predict := predict +
                     predict(combinedModels[[j]]$model, predictorDataM,
                             missing = NA)]
    }
    
    # Use the top M rank as the tiebreaker and order the predictions
    testFeatures[,predict := predict + topMPredict*topMPredictFactor]
    testFeatures[,order_predict := match(-predict, sort(-predict)), by=row_id]
    
    # Combine the top three neighbors to a string vector
    placeString <- paste(testFeatures[order_predict==1, place_id],
                         testFeatures[order_predict==2, place_id],
                         testFeatures[order_predict==3, place_id])
    
    # Check for ties in the ordering (should not occur)
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
