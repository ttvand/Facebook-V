# Logic that combines the predictions of all selected models for blending

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(nnet)

# Train range: 131:160 (161:170? IF time left)
# Validation range: c(1:4, 21:30)
# Test range: 1:287 #seq(225,221,-1)

# Processed ids
ids <- 161:170
nbIds <- length(ids)

# Execution time range in 24 hour notation (option to start at certain hour)
timeRange <- c(0,24)

# What data group is predicted?
blockTarget <- c("train", "validation", "test")[3]
targetDataExtension <- ifelse(blockTarget!="test", "train", "test")

# Considered prediction columns from the features data
predictionsColumns <- unique(
  c("placeMatch", "row_id", "actual_place_id", "place_id", "topMRank",
    "topMPredict", "timeSinceSummary", "x", "y", "accuracy",
    "regionDensity", "isAcGroup1", "isAcGroup2", "isAcGroup3",
    "distanceXLowBorder", "distanceXHighBorder", "distanceYLowBorder",
    "distanceYHighBorder", "madX", "madY", "madRelAc", "medX", "medY",
    "medAc", "madAc", "medRelAc", "madRelAc", "slightlyRelaxedMadRatio")
)

# External path to load predictions from and store to
loadStoreExternalHD <- FALSE
externalPredictionsFolder <- "H:/First level combined predictions"
externalFeaturesFolder <- "H:/Facebook features"

# Target date
targetDate <- "23-05-2016"

# Source the base prediction functions
source(file.path(getwd(), "xgboost", "predictXgboost.R"))
source(file.path(getwd(), "nnet", "predictNnet.R"))


# Wait for a valid time range
while(TRUE){
  exit <- as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
    as.numeric(format(Sys.time(),"%H")) <= timeRange[2]
  
  if(exit){
    break
  }
  
  # Display sleep message
  cat("Waiting for a valid time range", timeRange, "\n")
  
  # Sleep since the features file is not available
  Sys.sleep(300) 
}

#####################################################
# Find the path to the selected models for blending #
#####################################################

# List the path to the models that are being blended
blendFolders <- grep("Selected models for blending$", list.dirs(getwd()),
                     value = TRUE)

# Extract the blended model locations and names
blendModels <- c()
for(i in 1:length(blendFolders)){
  blendFolder <- blendFolders[i]
  files <- list.files(blendFolder)
  if(length(files) > 0){
    out <- paste0(blendFolder, "/", files)
    names(out) <- files
    blendModels <- c(blendModels, out)
  }
}
blendModelNames <- gsub(" |[.]rds$", "", names(blendModels))


# Load the blended models
nbModels <- length(blendModelNames)
models <- lapply(blendModels, readRDS)

# Loop over the processed ids and add the fitted model predictions
for(i in 1:nbIds){
  # Extract the processed id
  id <- ids[i]
  
  # Display progress message
  cat("\nFitting models for", blockTarget, "batch", id, "@",
      as.character(Sys.time()), "\n")
  
  # Set up the path to the features file
  if(loadStoreExternalHD){
    featuresFolder <- externalFeaturesFolder
    predictionsFolder <- externalPredictionsFolder
  } else{
    featuresFolder <- paste0("../Feature engineering/", targetDate, "/",
                             targetDataExtension)
    predictionsFolder <- file.path(getwd(), "Combined predictions")
  }
  
  # Extract the predictions path
  batchExtension <- paste0(blockTarget, " features - Random batch - ", id,
                           ".rds")
  predictionsPath <- file.path(predictionsFolder, batchExtension)
  
  # Load or extract the base features and predictions
  if(file.exists(predictionsPath)){
    predictions <- readRDS(predictionsPath)
    
    # Add the new common prediction columns
    newCommonPredict <- setdiff(predictionsColumns, names(predictions))
    if(length(newCommonPredict) > 0){
      # Load the features
      featuresPath <- file.path(featuresFolder, batchExtension)
      features <- readRDS(featuresPath)
      
      predictions <- cbind(predictions,
                           features[, newCommonPredict, with=FALSE])
      
      # Reorder the prediction columns
      targetColumns <- c(predictionsColumns,
                         setdiff(names(predictions), predictionsColumns))
      predictions <- predictions[, targetColumns, with=FALSE]
    }
  } else{
    # Load the features
    featuresPath <- file.path(featuresFolder, batchExtension)
    features <- readRDS(featuresPath)
    
    # Extract the common predictions columns
    fixedPredictCols <-
      predictionsColumns[predictionsColumns %in% names(features)]
    predictions <- features[, fixedPredictCols, with=FALSE]
  }
  
  # List the predict columns that need to be added
  addPredictCols <- setdiff(blendModelNames, names(predictions))
  nbAddPredict <- length(addPredictCols)
  
  # Add the predictions if they are not all present yet
  if(!file.exists(predictionsPath) || nbAddPredict > 0){
    # Load the features if they were not loaded before
    if(file.exists(predictionsPath)){
      featuresPath <- file.path(featuresFolder, batchExtension)
      features <- readRDS(featuresPath)
    }
    
    # Add the predictions
    for(j in 1:nbAddPredict){
      # Progress message
      cat("Adding fitted predictions for model", j, "of", nbAddPredict, "\n")
      
      # Extract the model id
      modelId <- which(addPredictCols[j] == blendModelNames)
      
      # Extract the model features
      featuresConsideredModel <- models[[modelId]]$predictors
      
      # Set up the name of the appropriate predict function
      learnerType <- unlist(strsplit(blendModelNames[modelId], "-"))[[1]]
      predictFunction <- paste0("predict",
                                toupper(substr(learnerType, 1, 1)),
                                substring(learnerType, 2))
      
      # Calculate the considered model predictions
      predictionsConsideredModel <-
        do.call(predictFunction,
                list(features = features[, featuresConsideredModel, with=FALSE],
                     trainModel = models[[modelId]]))
      
      # Add the predictions column
      colname <- blendModelNames[modelId]
      predictions[, (colname) := predictionsConsideredModel]
    }
    
    # Save the predictions
    saveRDS(predictions, predictionsPath)
  }
}