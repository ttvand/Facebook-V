# XGBOOST first level learner
# Learn and save a model given
#   - The train batch path
#   - The train features
#   - The considered top L places for each modeled check-in
#   - The learning hyperparameters

hyperParStringXGB <- function(hyperpar, topL, predictors){
  paste(paste(length(predictors), "features"),
        paste0("L", topL),
        paste(hyperpar$nrounds, "rounds"),
        paste(hyperpar$etaC, "etaC"),
        paste(hyperpar$subsample, "rowS"),
        paste(hyperpar$colsample_bytree, "colS"),
        paste(hyperpar$max.depth, "treeDepth"), sep = " - ")
}

generateXgboost <- function(trainId, loadFromExternalHD, predictors, topL,
                            hyperpar){
  
  # Path to store the generated model
  saveDir <- file.path(getwd(), "xgboost", "Models")
  savePath <- file.path(saveDir, paste0("xgboost - ",
                                        paste("train", trainId), " - ",
                                        hyperParStringXGB(hyperpar, topL,
                                                          predictors),
                                        ".rds"))
  if(!file.exists(savePath)){
    
    # Set up the path to the train features
    if(loadFromExternalHD){
      featuresDir <- "H:/Facebook features"
    } else{
      featuresDir <- paste0("../../Feature engineering/", "23-05-2016", "/",
                            "train")
    }
    featuresPath <- file.path(featuresDir,
                              paste0("train features - Random batch - ",
                                     trainId, ".rds"))
    
    # Set the considered target variable
    targetVar <- "placeMatch"
    
    # Read in the features
    features <- readRDS(featuresPath)
    
    # Set the top M rank if it is not available in the train features
    # Warnings suppressed because not all row ids have 100 places
    if(!"topMRank" %in% names(features)){
      suppressWarnings(features[, topMRank := 1:100, by=row_id])
    }
    
    # Subset on the top L places (based on top M rank)
    features <- features[topMRank <= topL]
    
    # Extract predictors data and labels from the features data
    predictorData <- features[, predictors, with=FALSE]
    labels <- features[, targetVar, with=FALSE][[1]]
    
    # Convert the predictor data to a matrix
    predictorDataM <- as.matrix(predictorData)
    
    # Learn the model
    model <- xgboost(data = predictorDataM, label = labels,
                     eta = hyperpar$etaC/hyperpar$nrounds,
                     nrounds = hyperpar$nrounds,
                     subsample = hyperpar$subsample,
                     colsample_bytree = hyperpar$colsample_bytree,
                     max.depth = hyperpar$max.depth,
                     objective = "binary:logistic",
                     eval_metric = "map",
                     missing = NA)
    
    # Write the model to the target save file
    saveRDS(list(model=model, predictors=predictors, hyperpar=hyperpar),
            savePath)
  }
}

# #############
# # Test case #
# #############
# 
# # Load the required libraries
# library(xgboost)
# library(data.table)
# 
# # Set working directory
# setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")
# 
# # Read in the ordered features
# features <- readRDS("orderedFeatures.rds")
# 
# trainId <- 26
# topL <- 20
# nrounds <- 200
# hyperpar <- list(nrounds = nrounds, etaC = 12, subsample = 0.5,
#                  colsample_bytree = 0.6, max.depth = 11)
# generateXgboost(trainId, TRUE, features, topL, hyperpar)