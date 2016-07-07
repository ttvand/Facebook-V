# XGBOOST first level learner

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners/Simple xgboost blend")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)
library(Ckmeans.1d.dp)
library(beepr)

# Target date
targetDate <- "23-05-2016"

# Train range: 26:45
ids <- 30:45
nbIds <- length(ids)

# Option to load the joined features file from an external HD.
loadFromExternalHD <- TRUE
externalHDPath <- "H:/Facebook features"

# Show variable importance plot
showVariableImportance <- FALSE

# Exclude records that did not contain a match in the top M observations?
# STRONGLY NOT recommended - performance decline for slightly faster execution
excludeNonTopMCandidates <- FALSE

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
hyperpar <- list(nrounds = nrounds, eta = 12/nrounds, subsample = 0.5,
                 colsample_bytree = 0.6, max.depth = 11)

# List the considered predictors and target variable
targetVar <- "placeMatch"
nbKs <- 10
nbMidKs <- 40
nbDifferentKCounts <- 7

# All predictors
predictors <- c(paste0("k", 1:(nbKs*nbDifferentKCounts))
                , paste0("kInt", 1:nbKs)
                , paste0("midK", 1:nbMidKs)
                , paste0("neighborDistanceMidK", 1:nbMidKs)
                , paste0("meanTimeDiff", 1:(nbKs*nbDifferentKCounts))
                , paste0("neighborDistance", 1:(nbKs*nbDifferentKCounts))
                , "timeSinceSummary" # DANGEROUS predictor wrt confounding!
                , "logTimeSinceSummaryDays"
                , "noCheckTimeSumEnd"
                , "relativeAccuracyZ", "logRelativeAccuracyZ"
                , "relativeRelaxedAccuracyZ", "logRelativeRelaxedAccuracyZ"
                , "relativeSlightlyRelaxedAccuracyZ"
                , "logRelativeSlightlyRelaxedAccuracyZ"
                , "madRelAc", "madRelLogAc",
                "relaxedMadRelLogAc", "slightlyRelaxedMadRelLogAc"
                , "relativeCount", "nonZeroDailyRelativeCount",
                "relativeRecentCount"
                # , "count", "nonZeroDailyCount"
                # , "countLast175D", "countLast70D", "countLast35D"
                , "hourQuarterDens", "hourQuarterDensR",
                "smoothHourQuarterDens", "smoothHourQuarterDensR"
                , "hourDens", "hourDensR", "smoothHourDens", "smoothHourDensR"
                , "dayDens", "dayDensR", "smoothDayDens", "smoothDayDensR"
                , "hourDayDens", "hourDayDensR", "smoothHourDayDens",
                "smoothHourDayDensR"
                , "x", "y", "xy"
                , "madX", "relaxedMadX", "slightlyRelaxedMadX"
                , "madY", "relaxedMadY", "slightlyRelaxedMadY"
                , "sdX", "relaxedSdX", "slightlyRelaxedSdX"
                , "sdY", "relaxedSdY", "slightlyRelaxedSdY"
                , "xZ", "xZLog", "xZLog2", "yZ", "yZLog", "yZLog2"
                , "xZSlightRelax", "yZSlightRelax"
                , "relaxedMadRatio", "logRelaxedMadRatio"
                , "slightlyRelaxedMadRatio", "logSlightlyRelaxedMadRatio"
                , paste0("periodDensity", 0:27)
                , paste0("weekBackDensity", seq(75,1,-2))
                , paste0("periodWeekDensity", 2:27)
                , "weeklyCount"
                , "distanceXBorder", "distanceYBorder"
                , "distanceXLowBorder", "distanceXHighBorder"
                , "distanceYLowBorder", "distanceYHighBorder"
                , "timeOpen", "isAtLeastOpenYear"
                , "dayDens52WeekBack", "smoothGDayDens52WeekBack"
                , "smoothUDayDens52WeekBack"
                , "dayDens53WeekBack", "smoothGDayDens53WeekBack"
                , "week52Correlation", "week53Correlation"
                # , "smoothUDayDens53WeekBack"
                , "accuracy", "isAcGroup1", "isAcGroup2", "isAcGroup3"
                , "madXValAcGroup", "sdXAcGroup",
                "madYValAcGroup", "sdYAcGroup"
                , "xZAcGroup", "yZAcGroup"
                , "acGroup3Density", "acGroup3DensityR"
                , "acGroup32Density", "acGroup32DensityR"
                , "acGroup3RecDensity", "acGroup3RecDensityR"
                , "acGroup32RecDensity", "acGroup32RecDensityR"
                , "regionDensity", "smoothRegionDensity"
                , "predictWeekDensityArima", "predictWeekDensitySes"
                , "predictWeekDensityHolt"
)

# Check for accidental duplicates
if(length(predictors) != length(unique(predictors))) browser()

# Drop unimportant features
dropPredictors <- c(
  # paste0("kInt", 1:10)
  # , paste0("k", 7:10)
  # , paste0("k", 17:20)
  # , paste0("k", 27:30)
  # , paste0("meanTimeDiff", 7:10)
  # , paste0("meanTimeDiff", 17:20)
  # , paste0("meanTimeDiff", 27:30)
  # , paste0("weekBackDensity", seq(75,1,-2)[-12])
  # , paste0("periodDensity", 0:27)
  # , paste0("meanTimeDiff", 1:(nbKs*nbDifferentKCounts))
  # paste0("neighborDistance", 1:(nbKs*nbDifferentKCounts))
  paste0("neighborDistance", rep(10*(1:(nbDifferentKCounts-1)),
                                 each=10) + rep(1:10,nbDifferentKCounts-1))
)
predictors <- setdiff(predictors, dropPredictors)


######################################################################

# Load the train places 
trainPlaces <- readRDS(paste0("../../Downsampling/", targetDate,
                              "/summary.rds"))
trainPlaces <- trainPlaces[, .N, by=place_id]

# Loop over all blocks and generate the xgboost learners
for(i in 1:nbIds){
  id <- ids[i]
  
  # Display progress message
  cat("Learning xgboost model for", blockTarget, "batch",
      id , "@", as.character(Sys.time()), "\n")
  
  # Path to store the generated model
  saveDir <- file.path(getwd(), targetDate)
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
      featuresDir <- paste0("../../Feature engineering/", targetDate, "/",
                            blockTarget)
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
    
    # setdiff(names(features), predictors)
    # setdiff(predictors, names(features))
    # which(unlist(lapply(features,function(x) any(is.na(x)))))
    
    # Optionally exclude records that did not contain a top M observation
    if(excludeNonTopMCandidates){
      features[,hasTopM := max(as.numeric(actual_place_id==top_place_id)),
               by=row_id]
      features <- features[hasTopM==TRUE]
      features[,hasTopM:=NULL]
    }
    
    # Extract predictors data and labels from the features data
    predictorData <- features[, predictors, with=FALSE]
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
    
    # # Assess the training accuracy (in-bag-error)
    # features[,predict := predict(model, predictorDataM, missing=NA)]
    # features[,place_predict := place_id[which.max(predict)], by=row_id]
    # features[,order_predict := match(1:length(predict),
    #                                  order(-predict)), by=row_id]
    
    # # What is the order of the predicted ids?
    # nbPlaces <- length(unique(features[,row_id]))
    # orderPredict <- features[placeMatch==TRUE, order_predict]
    # table(orderPredict)
    
    # Entitlement: predict all places that are available in the train data
    # Places that are not present in the train data are filtered at the
    # feature generation phase!
    
    # # Display the training accuracy - Commented since this gives a wrong
    # # image but good for initial assessment
    # cat("Mean top class training %:",
    #     round(sum(orderPredict==1)/nbPlaces*100,2),"\n")
    # cat("Mean average precision @3 :",
    #     round(sum((orderPredict==1) + (orderPredict==2)/2 +
    #                    (orderPredict==3)/3)/nbPlaces*100,2),"\n")
    
    # Assess feature importance
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