# XGBOOST second level learner

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Second level learners")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(Ckmeans.1d.dp)
library(beepr)

# Train ids to fit the second tier learners to
# Train range: 131:150
ids <- 161:170 #131:160
nbIds <- length(ids)

# Option to load the joined features file from an external HD.
loadFromExternalHD <- FALSE
externalHDPath <- "H:/First level combined predictions"

# Show variable importance plot
showVariableImportance <- FALSE

# What data group is trained? - Always train
blockTarget <- "train"

# Wait for the train data if it is not available yet
sleepIfNotExists <- TRUE

# Overwrite target file if it exists?
overwrite <- TRUE

# Execution time range in 24 hour notation (option to start fit at certain hour)
timeRange <- c(0,24)

# Xgboost hyperparameters
nrounds <- 2e2
hyperpar <- list(nrounds = nrounds, eta = 8/nrounds, subsample = 0.5,
                 colsample_bytree = 0.6, max.depth = 7)

# List the considered predictors and target variable
targetVar <- "placeMatch"
predictorSelection <- c("drop", "topFeatures")[1]
nbTopFeatures <- 103
dropPredictors <- c("placeMatch",
                    "topMRank",
                    "topMPredict",
                    "row_id", "actual_place_id", "place_id"
                    
                    # , "x", "y", "accuracy",
                    # "timeSinceSummary", "regionDensity", "isAcGroup1",
                    # "isAcGroup2", "isAcGroup3", "distanceXLowBorder",
                    # "distanceXHighBorder", "distanceYLowBorder",
                    # "distanceYHighBorder", "madX", "madY", "madRelAc", "medX",
                    # "medY", "medAc", "madAc", "medRelAc",
                    # "slightlyRelaxedMadRatio"
)

# Ordered features by importance score over batches 131:140
orderedFeatures <-
  c("xgboost-train58-142features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train183-182features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train61-285features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train175-186features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train77-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train196-220features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train79-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train171-215features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train114-430features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train83-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train90-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train187-191features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train68-194features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train99-202features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train72-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train89-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train56-154features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train82-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train67-229features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train186-211features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train177-195features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train179-231features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train78-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train181-214features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train174-239features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train113-430features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train60-151features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train197-189features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train192-225features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train97-191features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train54-272features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train76-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train46-233features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train100-133features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train48-188features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train184-207features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train74-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train47-275features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train96-168features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train115-430features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train84-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train110-233features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train85-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train86-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train109-145features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train69-240features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train71-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train59-121features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train185-180features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train188-234features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train62-225features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train103-224features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train53-115features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train51-165features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train199-210features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train189-228features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train176-214features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train64-172features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train173-205features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train107-269features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train112-430features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train80-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train198-182features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "accuracy", "xgboost-train95-240features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train200-214features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train93-231features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train194-236features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train81-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train75-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train193-194features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train91-156features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train87-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train195-201features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train105-180features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train66-129features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train104-144features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train101-218features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train52-157features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train191-206features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train172-240features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train49-134features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train178-218features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train94-115features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train190-193features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train50-212features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train111-430features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train88-430features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train55-111features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train57-268features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train98-237features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train65-182features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train92-158features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train108-201features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train63-106features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train73-430features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train102-195features-L100-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train106-217features-L100-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "x", "xgboost-train180-199features-L20-500rounds-11etaC-0.5rowS-0.6colS-11treeDepth", 
    "y", "xgboost-train70-222features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth", 
    "xgboost-train182-220features-L20-500rounds-12etaC-0.5rowS-0.6colS-11treeDepth"
  )
nbTopFeatures <- min(c(nbTopFeatures, length(orderedFeatures)))

######################################################################

# Loop over all train batches and generate the xgboost learners
for(i in 1:nbIds){
  id <- ids[i]
  
  # Display progress message
  cat("Learning xgboost model for", blockTarget, "batch",
      id , "@", as.character(Sys.time()), "\n")
  
  # Path to store the generated model
  saveDir <- file.path(getwd(), "xgboost models")
  savePath <- file.path(saveDir, paste0("xgboost - Random batch - ",
                                        id, ".rds"))
  
  # Combine the features if the file does not exist or overwrite = TRUE
  if(overwrite || !file.exists(savePath)){
    ####################################################
    # Load required data files for feature combination #
    ####################################################
    
    # Path to the combined features file
    if(loadFromExternalHD){
      featuresDir <- externalHDPath
    } else{
      featuresDir <- paste0("../First level learners/Combined predictions")
    }
    
    # Load the combined features data
    featuresPath <- file.path(featuresDir, paste0(blockTarget,
                                                  " features - Random batch - ",
                                                  id, ".rds"))
    # Wait until the features file is available
    while(TRUE){
      exit <- file.exists(featuresPath) &&
        as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
        as.numeric(format(Sys.time(),"%H")) <= timeRange[2]
      
      if(exit){
        successfulExit <- FALSE
        try({
          features <- readRDS(featuresPath)
          successfulExit <- TRUE
        }, silent=TRUE)
        if(successfulExit){
          break
        }  
      }
      
      # Display sleep message
      cat(paste0("Train features not available, sleeping for five minutes @"),
          as.character(Sys.time()), "\n")
      
      # Sleep since the features file is not available
      Sys.sleep(300) 
    }
    
    # Extract predictors data and labels from the first level features data
    if(predictorSelection == "drop"){
      predictorData <- features[, setdiff(names(features), dropPredictors),
                                with=FALSE]
    } else{
      predictorData <- features[, orderedFeatures[1:nbTopFeatures],
                                with=FALSE]
    }
    labels <- features[, targetVar, with=FALSE][[1]]
    
    # Convert the predictor data to a matrix
    predictorDataM <- as.matrix(predictorData)
    
    # Learn the model
    model <- xgboost(data = predictorDataM, label = labels,
                     eta = hyperpar$eta, nrounds = hyperpar$nrounds,
                     subsample = hyperpar$subsample,
                     colsample_bytree = hyperpar$colsample_bytree,
                     max.depth = hyperpar$max.depth,
                     objective = "binary:logistic",
                     eval_metric = "map",
                     missing = NA)
    
    # Assess feature importance
    predictors <- names(predictorData)
    importanceMatrix <- xgb.importance(predictors, model = model)
    if(showVariableImportance){
      p <- xgb.plot.importance(importanceMatrix)
      print(p)
    }
    
    # Create the target save folder if it does not yet exist
    dir.create(saveDir, showWarnings = FALSE)
    
    # Write the model to the target save file
    saveRDS(list(model=model, predictors=predictors, hyperpar=hyperpar,
                 importanceMatrix = importanceMatrix), savePath)
  }
}

# Play a positive sound to celebrate that the model was learnt :)
beep(sound = "fanfare")