# Logic to combine the top N nearest neighbor features with the summary
# features. Learners of the combined features can then be constructed in a 
# subsequent phase.
# The options are restricted to the top M candidates where M<<N

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(xgboost)

# Target date
targetDate <- "23-05-2016"

# Processed ids
# Test range: 1:287
# Train range: 11-219 since 1-10 in candidate selection
# Validation range: 1:73
ids <- 161:170
nbIds <- length(ids)

# Execution time range in 24 hour notation (option to start fit at certain hour)
timeRange <- c(0,24)

# What data group is combined?
blockTarget <- c("train", "validation", "test")[1]
targetDataExtension <- ifelse(blockTarget=="test", "test", "train")

# Overwrite target file if it exists?
overwrite <- TRUE

# Considered distance constants
distanceConstants <- c(1, 2.5, 4, 5.5, 7, 12, 30)
mainDistanceConstantId <- 2 # Important choice!?

# Top M - only retain top M candidates in order to focus on the signal and
# reduce storage space as well as calculation time in further steps
# topM << top N (100 vs 20?)
# Going from top N to top M reduces the candidates strongly but only loses 
# about 4-5% of the top places for N=100 and M=20
topM <- 20
addTopMProbs <- FALSE # Include top M model probabilities?
combineTopMLR <- FALSE # Use LR to combine the top M probabilities optimally?

# Option to load the KNN features from an external HD
loadFromExternalHD <- FALSE
externalNNFolder <- "H:/Facebook backup/Candidate selection"

# Option to save the joined features file to an external HD
saveToExternalHD <- TRUE
externalHDPath <- "H:/Facebook features"

# Add interpolated K counts
addInterpolKCount <- TRUE

# Add neighbor distances for other Ks?
addNeighborDistanceOtherKs <- FALSE

# Add middle KNN features
addMiddleNeighborFeatures <- TRUE
addNeighborDistanceMiddleKs <- TRUE

# Exclude batch records where the true class is not in the train data
# This does not apply to the feature combination of test data
excludeNewPlaces <- TRUE


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


# Load the topM LR combination model if used
if(combineTopMLR){
  topMLRModel <- readRDS("../First level learners/topM Tier II LR.rds")
}

# Distance constant derived variables
nbDistanceConstants <- length(distanceConstants)
otherIds <- (1:nbDistanceConstants)[-mainDistanceConstantId]
mainDistanceConstant <- distanceConstants[mainDistanceConstantId]
otherDistanceConstants <- setdiff(distanceConstants, mainDistanceConstant)
nbOtherDistanceConstants <- nbDistanceConstants - 1
orderDCCols <- c(mainDistanceConstantId, otherIds)
orderDCs <- distanceConstants[orderDCCols]

# Hour and week offset in minutes
dayHourOffset <- 0

# Week back features - 78 weeks in train data 
weekBackDensities <- seq(75,1,-2)
nbWeekbackDensities <- length(weekBackDensities)

# Relaxation constants
acGroup3Rel <- 3
acGroup32Rel <- 32
hourQuarterRel <- 96
hourRel <- 24
dayRel <- 14
hourDayRel <- 168

# Number of neighbor counts and number of considered neighbor counts
nbKs <- 10
nbConsideredKs <- 10 # Only consider high number neighbor counts (k>=50)?

# Z score additive constant in log feature calculation
addConstantZ <- 1

# Subfolder of target date that contains the models that are used to generate
# the top M selection. A simple blend of four advanced XGBOOST models at this
# point, trained on the topN count 
topMExtension <- "Top M models"


##############################################################################

# Set the path to the summary data
summaryPath <- file.path(getwd(), targetDate,
                         paste0(targetDataExtension, " summary features.rds"))

# Read in the summary data
summary <- readRDS(summaryPath)

# Load the raw summary data in order to obtain the training places
if(blockTarget != "test"){
  rawSummaryPath <- paste0("../Downsampling/", targetDate, "/summary.rds")
  rawSummary <- readRDS(rawSummaryPath)
  rawSummaryPlaces <- unique(rawSummary$place_id)
} else{
  rawSummaryPath <- paste0("../Data/", "/train.rds")
  rawSummary <- readRDS(rawSummaryPath)
}

# Load the top M models
topMFolder <- file.path("../First level learners", "Simple xgboost blend",
                        targetDate, topMExtension)
topMModelNames <- list.files(topMFolder)
nbTopMModels <- length(topMModelNames)
topMModels <- vector(mode = "list", length = nbTopMModels)
for(i in 1:nbTopMModels){
  # Load the model
  topMModels[[i]] <- readRDS(file.path(topMFolder, topMModelNames[i]))
}

# Loop over all blocks and combine the features
for(i in 1:nbIds){
  # Extract the train id
  id <- ids[i]
  
  # Display progress message
  cat("Combining features for", blockTarget, "batch", id, "@",
      as.character(Sys.time()), "\n")
  
  # Path to the combined features file
  if(saveToExternalHD){
    saveDir <- externalHDPath
  } else{
    saveDir <- file.path(getwd(), targetDate, targetDataExtension)
  }
  savePath <- file.path(saveDir, paste0(blockTarget,
                                        " features - Random batch - ",
                                        id, ".rds"))
  
  # Combine the features if the file does not exist or overwrite = TRUE
  if(overwrite || !file.exists(savePath)){
    ####################################################
    # Load required data files for feature combination #
    ####################################################
    
    # Load the considered batch
    filePath <- paste0("../Downsampling/", targetDate, "/",
                       blockTarget, " - Random batch - ",
                       id, ".rds")
    batch <- readRDS(filePath)
    
    # Load the top N features
    if(loadFromExternalHD){
      topNPath <- paste0(externalNNFolder, "/", targetDate, "/",
                         targetDataExtension, "/Top ", blockTarget,
                         " NN ", mainDistanceConstant, " - Random batch - ",
                         id, ".rds")
    } else{
      topNPath <- paste0("../Candidate selection/", targetDate, "/",
                         targetDataExtension, "/Top ", blockTarget,
                         " NN ", mainDistanceConstant, " - Random batch - ",
                         id, ".rds")
    }
    topNBatch <- readRDS(topNPath)
    
    # Load time trend features
    weekTrendsPath <- file.path(getwd(), targetDate,
                                "weekly summary features.rds")
    weekTrends <- readRDS(weekTrendsPath)
    
    # Check if the candidates are valid
    # if(blockTarget != "test"){
    #   topNPlaces <- t(topNBatch[,1:100])
    #   dim(topNPlaces) <- c(length(topNPlaces), 1)
    #   sum(topNPlaces == rep(batch$place_id,each=100), na.rm = TRUE)
    # }
    
    ##############################
    # Join batch with candidates #
    ##############################
    
    # Extract the top N count
    topN <- (ncol(topNBatch)-nbKs)/(1+2*nbKs)
    
    # Rename the true place_id
    if(blockTarget != "test"){
      setnames(batch,"place_id","actual_place_id")
    }
    
    # Replicate the batch data table topN times
    joined <- batch[rep(1:nrow(batch), each = topN)]
    
    ####################################
    # Join batch with summary features #
    ####################################
    
    # Add the candidate place ids
    topNPlaces <- t(topNBatch[,1:topN])
    dim(topNPlaces) <- c(length(topNPlaces),1)
    joined[, top_place_id:=topNPlaces]
    summaryRows <- match(joined$top_place_id, summary$place_id)
    validSummaryRows <- summaryRows[!is.na(summaryRows)]
    validSummaryIds <- which(!is.na(summaryRows))
    
    # Drop hour and day mean columns from batch
    # Places with a count of less than 3 in the last 175 days are dropped
    joined <- cbind(joined[validSummaryIds],
                    summary[validSummaryRows,
                            !grepl("^hour|^day|^dailyDensity|^weekly.*Forecast",
                                   names(summary)),
                            with=FALSE])
    
    
    #####################################
    # Add observation specific features #
    #####################################
    
    # Convert time to hour of day, day of week and overall week 
    joined$hourQuarter <- floor(((joined$time + dayHourOffset) %% 1440)/15)
    joined$hour <- floor(((joined$time + dayHourOffset) %% 1440)/60)
    joined$day <-
      floor(((joined$time + dayHourOffset) %% (1440*7)) / 1440)
    joined$hourDay <-
      floor(((joined$time + dayHourOffset) %% (1440*7)) / 60)
    joined$totalDay <- floor((joined$time + dayHourOffset)/(1440))
    joined$week <- floor((joined$time + dayHourOffset)/(1440*7))
    
    # Add time since end of training period and time since last observation
    maxTimeSummary <- max(summary$maxTime)
    joined[,timeSinceSummary:=time-maxTimeSummary]
    joined[,logTimeSinceSummaryDays:=log(3+timeSinceSummary/1440)]
    joined[,noCheckTimeSumEnd:=maxTimeSummary-maxTime]
    
    # Set the predict horizon
    if(targetDataExtension == "train"){
      predictRange <- 58:77
    } else{
      predictRange <- 77:100
    }
    nbExtrapolatedWeeks <- length(predictRange)
    
    # Add extrapolated weekly densities
    matchPlaceIdsGeneral <- match(joined$place_id, summary$place_id)
    joined[, predictWeekDensityArima := -1]
    joined[, predictWeekDensitySes := -1]
    joined[, predictWeekDensityHolt := -1]
    for(i in 1:nbExtrapolatedWeeks){
      # Week for which the predictions are considered
      predictWeek <- predictRange[i]
      
      # Extract the extrapolated week ids
      weekIds <- which(joined$week == predictWeek)
      
      # Add the predictions for the specific predicted week
      matchPlaceIds <- matchPlaceIdsGeneral[weekIds]
      joined[weekIds, predictWeekDensityArima :=
               summary[[paste0("weeklyArimaForecast", predictWeek)]][
                 matchPlaceIds]
             ]
      joined[weekIds, predictWeekDensitySes :=
               summary[[paste0("weeklySesForecast", predictWeek)]][
                 matchPlaceIds]
             ]
      joined[weekIds, predictWeekDensityHolt :=
               summary[[paste0("weeklyHoltForecast", predictWeek)]][
                 matchPlaceIds]
             ]
    }
    
    # Add a feature indicating the time since the first checkin and a 
    # boolean flag that indicates if the place has been open for at least a
    # year (52*7 = 364 days)
    joined[,timeOpen := time-minTime]
    joined[,isAtLeastOpenYear := as.numeric(timeOpen >= (52*7*1440))]
    joined[,timeOpen := timeOpen/maxTimeSummary] # Make time open relative!
    
    # Add region density features
    regionDensityPath <- file.path("../Feature engineering", targetDate,
                                   paste(targetDataExtension,
                                         "region densities.rds"))
    regionDensities <- readRDS(regionDensityPath)
    
    # Calculate the region id for the joined observations
    nbXBlocks <- 100
    nbYBlocks <- 400
    joinedXLimited <- ifelse(joined$x<1e-6, 1e-6, joined$x)
    joinedYLimited <- ifelse(joined$y<1e-6, 1e-6, joined$y)
    regionBlockId <-  1 + floor((joinedYLimited-1e-7)*nbYBlocks/10) +
      nbYBlocks*floor((joinedXLimited-1e-7)*nbXBlocks/10)
    joined[,regionDensity :=
             as.numeric(regionDensities$density[regionBlockId])]
    joined[,smoothRegionDensity :=
             as.numeric(regionDensities$smoothDensity[regionBlockId])]
    
    # Normalize accuracy measure for time trend
    # relativeAccuracyZ <- (joined$accuracy/weekTrends[joined$week+1, madAc] -
    #                         joined$medRelAc)/(0.1+joined$madRelAc)
    relativeAccuracyZ <-
      abs(log(1+joined$accuracy)/log(1+weekTrends[joined$week+1, medAc]) -
            joined$medRelLogAc)/(0.1+joined$madRelLogAc)  
    joined[,relativeAccuracyZ := relativeAccuracyZ]
    joined[,logRelativeAccuracyZ := log(1+relativeAccuracyZ)]
    
    relativeRelaxedAccuracyZ <-
      abs(joined$accuracy/weekTrends[joined$week+1, madAc] -
            joined$relaxedMedRelLogAc)/(0.1+joined$relaxedMadRelLogAc)
    joined[,relativeRelaxedAccuracyZ := relativeRelaxedAccuracyZ]
    joined[,logRelativeRelaxedAccuracyZ := log(1+relativeRelaxedAccuracyZ)]
    relativeSlightlyRelaxedAccuracyZ <-
      abs(joined$accuracy/weekTrends[joined$week+1, madAc] -
            joined$slightlyRelaxedMedRelLogAc)/
      (0.1+joined$slightlyRelaxedMadRelLogAc)
    joined[,relativeSlightlyRelaxedAccuracyZ :=
             relativeSlightlyRelaxedAccuracyZ]
    joined[,logRelativeSlightlyRelaxedAccuracyZ :=
             log(1+relativeSlightlyRelaxedAccuracyZ)]
    
    # Add the weekly density predictor
    joined[,weeklyCount := weekTrends[joined$week+1,N]]
    
    ###############################
    # Add accuracy group features #
    ###############################
    
    # Add the accuracy group and sparse representation of the groups
    joined[,accuracyGroup3 := cut(accuracy, breaks=c(0,45,85,1e5))]
    joined[,accuracyGroup32 := cut(accuracy,
                                   breaks=c(0, 6, 10, 15, 20, 26, 31, 35, 40,
                                            46, 51, 55, 57, 59, 61, 63, 64,
                                            65, 66, 68, 69, 71, 73, 76, 81,
                                            96, 125, 157, 165, 173, 223, 424,
                                            1e5))]
    joined[,isAcGroup1 := as.numeric(as.numeric(accuracyGroup3)==1)]
    joined[,isAcGroup2 := as.numeric(as.numeric(accuracyGroup3)==2)]
    joined[,isAcGroup3 := as.numeric(as.numeric(accuracyGroup3)==3)]
    
    # Iterate over the accuracy groups and add the relative density and
    # group rescaled Z scores as well as the mad and sd for the group
    joined[, xZAcGroup := -1]
    joined[, madXValAcGroup := -1]
    joined[, sdXAcGroup := -1]
    joined[, yZAcGroup := -1]
    joined[, madYValAcGroup := -1]
    joined[, sdYAcGroup := -1]
    joined[, acGroup3Density := -1]
    joined[, acGroup3RecDensity := -1]
    joined[, acGroup3DensityR := -1]
    joined[, acGroup3RecDensityR := -1]
    for(j in 1:3){
      # Extract the group ids
      groupIds <- which(as.numeric(joined$accuracyGroup3)==j)
      
      # Add the relative density for the specific accuracy group
      matchPlaceIds <- matchPlaceIdsGeneral[groupIds]
      joined[groupIds, acGroup3Density := summary[[paste0("relAcGroup3-",j)]][
        matchPlaceIds]
        ]
      
      # Add the recent relative density for the specific accuracy group
      joined[groupIds,
             acGroup3RecDensity := summary[[paste0("relRecAcGroup3-",j)]][
               matchPlaceIds]
             ]
      
      # Add the relaxed relative density for the specific accuracy group
      joined[groupIds, acGroup3DensityR :=
               (acGroup3Density * summary[["count"]][
                 matchPlaceIds] + acGroup3Rel/3)/(
                   summary[["count"]][matchPlaceIds] +
                     acGroup3Rel)]
      
      # Add the recent relaxed relative density for the specific accuracy group
      joined[groupIds, acGroup3RecDensityR :=
               (acGroup3RecDensity * summary[["recentCount"]][
                 matchPlaceIds] + acGroup3Rel/3)/(
                   summary[["recentCount"]][matchPlaceIds] +
                     acGroup3Rel)]
      
      # Add the xZ and yZ scores for the specific accuracy group
      joined[groupIds, xZAcGroup := abs(x-summary[[paste0("medXAcGroup",j)]][
        matchPlaceIds])/
          (0.001 + summary[[paste0("madXAcGroup",j)]][matchPlaceIds])
        ]
      joined[groupIds, yZAcGroup := abs(y-summary[[paste0("medYAcGroup",j)]][
        matchPlaceIds])/
          (0.0005 + summary[[paste0("madYAcGroup",j)]][matchPlaceIds])
        ]
      
      # Add the mad and sd for the specific accuracy group
      joined[groupIds,
             madXValAcGroup := summary[[paste0("madXAcGroup",j)]][matchPlaceIds]]
      joined[groupIds,
             sdXAcGroup := summary[[paste0("sdXAcGroup",j)]][matchPlaceIds]]
      joined[groupIds,
             madYValAcGroup := summary[[paste0("madYAcGroup",j)]][matchPlaceIds]]
      joined[groupIds,
             sdYAcGroup := summary[[paste0("sdYAcGroup",j)]][matchPlaceIds]]
    }
    
    joined[, acGroup32Density := -1]
    joined[, acGroup32RecDensity := -1]
    joined[, acGroup32DensityR := -1]
    joined[, acGroup32RecDensityR := -1]
    for(j in 1:32){
      # Extract the group ids
      groupIds <- which(as.numeric(joined$accuracyGroup32)==j)
      
      # Add the relative density for the specific accuracy group
      matchPlaceIds <- matchPlaceIdsGeneral[groupIds]
      joined[groupIds, acGroup32Density :=
               summary[[paste0("relAcGroup32-",j)]][matchPlaceIds]
             ]
      
      # Add the relative recent density for the specific accuracy group
      joined[groupIds, acGroup32RecDensity :=
               summary[[paste0("relRecAcGroup32-",j)]][matchPlaceIds]
             ]
      
      # Add the relaxed relative density for the specific accuracy group
      joined[groupIds, acGroup32DensityR :=
               (acGroup32Density * summary[["count"]][
                 matchPlaceIds] + acGroup32Rel/32)/(
                   summary[["count"]][matchPlaceIds] +
                     acGroup32Rel)]
      
      # Add the relaxed recent relative density for the specific accuracy group
      joined[groupIds, acGroup32RecDensityR :=
               (acGroup32RecDensity * summary[["recentCount"]][
                 matchPlaceIds] + acGroup32Rel/32)/(
                   summary[["recentCount"]][matchPlaceIds] +
                     acGroup32Rel)]
    }
    
    # Remove accuracy groups
    joined[, accuracyGroup3 := NULL]
    joined[, accuracyGroup32 := NULL]
    
    ##################################
    # Add batch - candidate features #
    ##################################
    
    # Add a flag that indicates if the top place matches the actual place
    # This is the target variable of the binary classification 
    if(blockTarget != "test"){
      joined[,placeMatch:=as.numeric(actual_place_id == top_place_id)]
    }
    
    # Add nearest neighbor features (counts and mean time difference)
    
    # Add the K counts
    for(j in 1:nbConsideredKs){
      colname <- paste0("k",j)
      val <- as.numeric(t(topNBatch[,topN*j+(1:topN)]))[validSummaryIds]
      dim(val) <- c(length(val),1)
      joined[, (colname) := val]
    }
    
    # Add the mean time difference counts as a RELATIVE fraction so that
    # the interpretation is the same for the test set
    for(j in 1:nbConsideredKs){
      colname <- paste0("meanTimeDiff",j)
      val <- as.numeric(t(topNBatch[,topN*(j+nbKs)+
                                      (1:topN)]))[validSummaryIds]/
        maxTimeSummary # This divisor makes it a relative fraction
      dim(val) <- c(length(val),1)
      joined[, (colname) := val]
    }
    
    # Add the relative difference to the Kth neighbors, relative to the
    # summary count
    for(j in 1:nbConsideredKs){
      colname <- paste0("neighborDistance",j)
      val <- 1e-12*rep(as.numeric(topNBatch[,topN*2*(nbKs+1/2)+j]),
                       each=topN)[validSummaryIds]*
        nrow(rawSummary) # This multiplier makes it a relative measure
      dim(val) <- c(length(val),1)
      joined[, (colname) := val]
    }
    
    # Optionally add the mid-k nearest neighbor features
    if(addMiddleNeighborFeatures){
      # Load the mid K raw data
      if(loadFromExternalHD){
        midKRawPath <- paste0(externalNNFolder, "/", targetDate, "/",
                              targetDataExtension, "/Top ", blockTarget,
                              " mid NN - Random batch - ", id, ".rds")
      } else{
        midKRawPath <- paste0("../Candidate selection/", targetDate, "/",
                              targetDataExtension, "/Top ", blockTarget,
                              " mid NN - Random batch - ", id, ".rds")
      }
      midKRaw <- readRDS(midKRawPath)
      
      # Calculate the relevant constants in the mid K analysis
      distanceKRaw <- unique(midKRaw[,1])
      nbDistanceCMidK <- length(distanceKRaw)
      nbMidKs <- floor(ncol(midKRaw)/topN) - 1
      
      # Count the ids of new observations
      newIds <- c(1, 1+which(joined$row_id[-nrow(joined)]!=joined$row_id[-1]))
      
      # Loop over the distance constants and match the NN counts and NN
      # distances
      for(j in 1:nbDistanceCMidK){
        midKRawDc <- midKRaw[midKRaw[,1] == distanceKRaw[j],]
        
        # Match the mid K place ids with the main distance constant place ids
        topNMidKPlaces <- midKRawDc[, 1+(1:topN)]
        
        matchIds <- rep(NA, nrow(joined))
        startId <- 1
        for(k in 1:length(newIds)){
          if(k==length(newIds)){
            endId <- nrow(joined)
          } else{
            endId <- newIds[k+1]-1
          }
          matchIds[startId:endId] <- (k-1)*topN +
            match(joined$top_place_id[startId:endId], topNMidKPlaces[k,])
          startId <- endId + 1
        }
        
        # Add the matched K counts for the mid K NN
        for(k in 1:nbMidKs){
          colname <- paste0("midK", k + (j-1)*nbMidKs)
          val <- suppressWarnings(as.numeric(t(
            midKRawDc[,1 + topN*k+(1:topN)])))
          dim(val) <- c(length(val),1)
          
          # Match the values with the right place id and replace missing
          # matches with a zero count
          matchedVal <- val[matchIds]
          matchedVal[is.na(matchedVal)] <- 0
          joined[, (colname) := matchedVal]
        }
        
        # Optionally add the neighbor distances for the middle K's
        if(addNeighborDistanceMiddleKs){
          for(k in 1:nbMidKs){
            colname <- paste0("neighborDistanceMidK", k + (j-1)*nbMidKs)
            val <- 1e-12*rep(as.numeric(midKRawDc[, 1+topN*(1+nbMidKs) + k]),
                             each=topN)[validSummaryIds]*
              nrow(rawSummary) # This multiplier makes it a relative measure
            dim(val) <- c(length(val), 1)
            joined[, (colname) := val]
          }
        }
      }
    }
    
    # Load NN for other distance constants
    if(nbOtherDistanceConstants>0){
      # Count the ids of new observations
      newIds <- c(1,1+which(joined$row_id[-nrow(joined)]!=joined$row_id[-1]))
      
      for(j in 1:nbOtherDistanceConstants){
        # Load the top N features
        if(loadFromExternalHD){
          topNOtherPath <- paste0(externalNNFolder, "/", targetDate, "/",
                                  targetDataExtension, "/Top ", blockTarget,
                                  " NN ", otherDistanceConstants[j],
                                  " - Random batch - ", id, ".rds")
        } else{
          topNOtherPath <- paste0("../Candidate selection/", targetDate, "/",
                                  targetDataExtension, "/Top ", blockTarget,
                                  " NN ", otherDistanceConstants[j],
                                  " - Random batch - ", id, ".rds")
        }
        
        topNOtherBatch <- readRDS(topNOtherPath)
        
        # Match the other place ids with the main distance constant place ids
        topNOtherPlaces <- topNOtherBatch[,1:topN]
        # dim(topNOtherPlaces) <- c(length(topNOtherPlaces),1)
        # topNOtherPlaces <- topNOtherPlaces[validSummaryIds]
        
        matchIds <- rep(NA, nrow(joined))
        startId <- 1
        for(k in 1:length(newIds)){
          if(k==length(newIds)){
            endId <- nrow(joined)
          } else{
            endId <- newIds[k+1]-1
          }
          matchIds[startId:endId] <- (k-1)*topN +
            match(joined$top_place_id[startId:endId], topNOtherPlaces[k,])
          startId <- endId + 1
        }
        
        # # Add the other place ids to summary
        # otherPlaceCol <- paste0("top_place_id_other",j)
        # joined[,(otherPlaceCol) := topNOtherPlaces]
        # 
        # # Match the other top place ids with the main top place ids
        # joined[,matchTopPlaceOther :=
        #          match(get(otherPlaceCol), top_place_id), by=actual_place_id]
        
        # Add the matched K counts for the other NN
        for(k in 1:nbConsideredKs){
          colname <- paste0("k", k + j*nbKs)
          val <- suppressWarnings(as.numeric(t(
            topNOtherBatch[,topN*k+(1:topN)])))
          dim(val) <- c(length(val),1)
          
          # Match the values with the right place id and replace missing
          # matches with a zero count
          matchedVal <- val[matchIds]
          matchedVal[is.na(matchedVal)] <- 0
          joined[, (colname) := matchedVal]
        }
        
        # Add the mean time differences
        for(k in 1:nbConsideredKs){
          colname <- paste0("meanTimeDiff", k + j*nbKs)
          val <- suppressWarnings(as.numeric(t(
            topNOtherBatch[,topN*(k+nbKs)+(1:topN)]))/
              maxTimeSummary) # This divisor makes it a relative fraction
          dim(val) <- c(length(val),1)
          
          # Match the values with the right place id and replace missing
          # matches with the mean time difference of the highest K count
          matchedVal <- val[matchIds]
          meanTimeDiffNonNa <- mean(matchedVal[!is.na(matchedVal)])
          matchedVal[is.na(matchedVal)] <- meanTimeDiffNonNa
          joined[, (colname) := matchedVal]
        }
        
        # Optionally add the relative difference to the Kth neighbors, relative 
        # to the summary count
        if(addNeighborDistanceOtherKs){
          for(k in 1:nbConsideredKs){
            colname <- paste0("neighborDistance",k + j*nbKs)
            val <- 1e-12*rep(as.numeric(topNOtherBatch[,topN*2*(nbKs+1/2)+k]),
                             each=topN)[validSummaryIds]*
              nrow(rawSummary) # This multiplier makes it a relative measure
            dim(val) <- c(length(val),1)
            joined[, (colname) := val]
          }
        }
      }
      
      # Optionally Add interpolated KNN counts - MAD RATIO OR RELAXED MAD
      # RATIO???
      if(addInterpolKCount){
        interpolRatio <- joined$ratioMadXY #relaxedRatioMadXY
        for(k in 1:nbConsideredKs){
          # Assign space for the interpolated counts
          intVal <- rep(NA, nrow(joined))
          
          # Set the extreme interpolated counts
          lowIds <- which(interpolRatio<distanceConstants[1])
          highIds <- which(interpolRatio>=distanceConstants[nbDistanceConstants])
          intVal[lowIds] <-
            joined[lowIds, paste0("k", k + (orderDCCols[1]-1)*nbKs),
                   with=FALSE][[1]]
          highKCol <- k + (orderDCCols[nbDistanceConstants]-1)*nbKs
          intVal[highIds] <-
            joined[highIds, paste0("k", k +
                                     (orderDCCols[nbDistanceConstants]-1)*
                                     nbKs), with=FALSE][[1]]
          
          # Interpolate the non-extreme counts
          for(intCount in 1:nbOtherDistanceConstants){
            intIds <- which(interpolRatio>=distanceConstants[intCount] &
                              interpolRatio<distanceConstants[intCount+1])
            lowInt <- joined[intIds,
                             paste0("k", k + (orderDCCols[intCount]-1)*nbKs),
                             with=FALSE][[1]]
            highInt <- joined[intIds,
                              paste0("k", k + (orderDCCols[intCount+1]-1)*nbKs),
                              with=FALSE][[1]]
            lowPart <- 
              (distanceConstants[intCount+1] - interpolRatio[intIds])/
              (distanceConstants[intCount+1] - distanceConstants[intCount])
            intVal[intIds] <- lowPart*lowInt + (1-lowPart)*highInt
          }
          
          # Assign the interpolated counts
          intCol <- paste0("kInt", k)
          joined[, (intCol) := intVal]
        }
      }
    }
    
    # Extract the hourly quarter densities
    # (relaxed + unrelaxed) * (smoothed/non-smoothed)
    joined[,hourQuarterDens := 1/96]
    # matchPlaceIdsGeneral <- match(joined$place_id, summary$place_id)
    for(j in 0:95){
      matchIds <- !is.na(matchPlaceIdsGeneral) & joined$hourQuarter==j
      matchPlaceIds <- matchPlaceIdsGeneral[matchIds]
      joined[which(matchIds),
             hourQuarterDens := summary[[paste0("hourQuarterMean",j)]][
               matchPlaceIds]
             ]
      joined[which(matchIds),
             hourQuarterDensR := (summary[[paste0("hourQuarterMean",j)]][
               matchPlaceIds]*summary[matchPlaceIds,count] + hourQuarterRel/24)/
               (hourQuarterRel + summary[matchPlaceIds,count])
             ]
      joined[which(matchIds), smoothHourQuarterDens :=
               summary[[paste0("hourQuarterMeanS",j)]][matchPlaceIds]
             ]
      joined[which(matchIds), smoothHourQuarterDensR :=
               (summary[[paste0("hourQuarterMeanS",j)]][
                 matchPlaceIds]*summary[matchPlaceIds,count] +
                  hourQuarterRel/24)/
               (hourQuarterRel + summary[matchPlaceIds,count])
             ]
    }
    
    # Extract the hourly densities
    # (relaxed + unrelaxed) * (smoothed/non-smoothed)
    joined[,hourDens := 1/24]
    # matchPlaceIdsGeneral <- match(joined$place_id, summary$place_id)
    for(j in 0:23){
      matchIds <- !is.na(matchPlaceIdsGeneral) & joined$hour==j
      matchPlaceIds <- matchPlaceIdsGeneral[matchIds]
      joined[which(matchIds), hourDens := summary[[paste0("hourMean",j)]][
        matchPlaceIds]
        ]
      joined[which(matchIds), hourDensR := (summary[[paste0("hourMean",j)]][
        matchPlaceIds]*summary[matchPlaceIds,count] + hourRel/24)/
          (hourRel + summary[matchPlaceIds,count])
        ]
      joined[which(matchIds), smoothHourDens :=
               summary[[paste0("hourMeanS",j)]][matchPlaceIds]
             ]
      joined[which(matchIds), smoothHourDensR :=
               (summary[[paste0("hourMeanS",j)]][
                 matchPlaceIds]*summary[matchPlaceIds,count] + hourRel/24)/
               (hourRel + summary[matchPlaceIds,count])
             ]
    }
    
    # Extract the daily densities and the smoothed daily densities
    joined[,dayDens := 1/7]
    for(j in 0:6){
      matchIds <- !is.na(matchPlaceIdsGeneral) & joined$day==j
      matchPlaceIds <- matchPlaceIdsGeneral[matchIds]
      joined[which(matchIds), dayDens :=
               summary[[paste0("dayMean",j)]][matchPlaceIds]
             ]
      joined[which(matchIds), dayDensR := (summary[[paste0("dayMean",j)]][
        matchPlaceIds]*summary[matchPlaceIds,count] + dayRel/7)/
          (dayRel + summary[matchPlaceIds,count])
        ]
      joined[which(matchIds), smoothDayDens :=
               summary[[paste0("dayMeanS",j)]][matchPlaceIds]
             ]
      joined[which(matchIds), smoothDayDensR :=
               (summary[[paste0("dayMeanS",j)]][
                 matchPlaceIds]*summary[matchPlaceIds,count] + dayRel/7)/
               (dayRel + summary[matchPlaceIds,count])
             ]
    }
    
    # Extract the hour-daily densities since time of day interact with day
    # of week wrt checkin counts!
    # (relaxed + unrelaxed) * (smoothed/non-smoothed)
    joined[,hourDayDens := 1/168]
    for(j in 0:167){
      matchIds <- !is.na(matchPlaceIdsGeneral) & joined$hourDay==j
      matchPlaceIds <- matchPlaceIdsGeneral[matchIds]
      joined[which(matchIds), hourDayDens := summary[[paste0("hourDayMean",j)]][
        matchPlaceIds]
        ]
      joined[which(matchIds), hourDayDensR := (summary[[paste0("hourDayMean",
                                                               j)]][
                                                                 matchPlaceIds]*summary[matchPlaceIds,count] + hourDayRel/168)/
               (hourDayRel + summary[matchPlaceIds,count])
             ]
      joined[which(matchIds), smoothHourDayDens :=
               summary[[paste0("hourDayMeanS",j)]][matchPlaceIds]
             ]
      joined[which(matchIds), smoothHourDayDensR :=
               (summary[[paste0("hourDayMeanS",j)]][
                 matchPlaceIds]*summary[matchPlaceIds,count] + hourDayRel/168)/
               (hourDayRel + summary[matchPlaceIds,count])
             ]
    }
    
    # Consider frequency of a year back - TODO - first add summary measures in
    # create features summary logic
    # Deprecated, no longer required (no yearly effect) FALSE!! THERE is an
    # effect!!!
    # Add daily densities of a year back
    joined[,dayDens52WeekBack := -1]
    joined[,smoothGDayDens52WeekBack := -1]
    joined[,smoothUDayDens52WeekBack := -1]
    
    joined[,dayDens53WeekBack := -1]
    joined[,smoothGDayDens53WeekBack := -1]
    # joined[,smoothUDayDens53WeekBack := -1]
    for(j in 35:343){
      # # 365 days back feature assignment
      # matchIds365 <- !is.na(matchPlaceIdsGeneral) & joined$totalDay==(j+365)
      # if(sum(matchIds365)>0){
      #   matchPlaceIds365 <- matchPlaceIdsGeneral[matchIds365]
      #   joined[which(matchIds365), dayDens365DaysBack :=
      #            summary[[paste0("dailyDensity",j)]][matchPlaceIds365]
      #          ]
      # }
      # 
      # # 366 days back feature assignment
      # matchIds366 <- !is.na(matchPlaceIdsGeneral) & joined$totalDay==(j+366)
      # if(sum(matchIds366)>0){
      #   matchPlaceIds366 <- matchPlaceIdsGeneral[matchIds366]
      #   joined[which(matchIds366), dayDens366DaysBack :=
      #            summary[[paste0("dailyDensity",j)]][matchPlaceIds366]
      #          ]
      # }
      
      # 52 week back features assignment
      matchIds52 <- !is.na(matchPlaceIdsGeneral) & joined$totalDay==(j+52*7)
      if(sum(matchIds52)>0){
        matchPlaceIds52 <- matchPlaceIdsGeneral[matchIds52]
        joined[which(matchIds52), dayDens52WeekBack :=
                 summary[[paste0("dailyDensity",j)]][matchPlaceIds52]
               ]
        joined[which(matchIds52), smoothGDayDens52WeekBack :=
                 summary[[paste0("dailyDensitySG",j)]][matchPlaceIds52]
               ]
        joined[which(matchIds52), smoothUDayDens52WeekBack :=
                 summary[[paste0("dailyDensitySU",j)]][matchPlaceIds52]
               ]
      }
      
      # 53 week back features assignment
      matchIds53 <- !is.na(matchPlaceIdsGeneral) & joined$totalDay==(j+53*7)
      if(sum(matchIds53)>0){
        matchPlaceIds53 <- matchPlaceIdsGeneral[matchIds53]
        joined[which(matchIds53), dayDens53WeekBack :=
                 summary[[paste0("dailyDensity",j)]][matchPlaceIds53]
               ]
        joined[which(matchIds53), smoothGDayDens53WeekBack :=
                 summary[[paste0("dailyDensitySG",j)]][matchPlaceIds53]
               ]
        # joined[which(matchIds53), smoothUDayDens53WeekBack :=
        #          summary[[paste0("dailyDensitySU",j)]][matchPlaceIds53]
        #        ]
      }
    }
    
    #############################
    # Create week back features #
    #############################
    
    # Generate initial values
    for(j in 1:nbWeekbackDensities){
      weekBackDensity <- weekBackDensities[j]
      colname <- paste0("weekBackDensity", weekBackDensity)
      joined[, (colname) := NA_real_]
    }
    
    lastSummaryDay <- floor((max(summary$maxTime) + dayHourOffset)/(1440))
    for(j in 407:707){
      # Week back features assignment
      matchIds <- !is.na(matchPlaceIdsGeneral) & joined$totalDay==j
      if(sum(matchIds)>0){
        matchPlaceIds <- matchPlaceIdsGeneral[matchIds]
        minWeekBack <- 1-floor((lastSummaryDay-j+1)/7)
        maxWeekBack <- floor((j-7)/7)
        validWeeksBack <- weekBackDensities[weekBackDensities >= minWeekBack &
                                              weekBackDensities <= maxWeekBack]
        nbValidWeeksBack <- length(validWeeksBack)
        
        # Update week back features that can be calculated
        for(k in 1:nbValidWeeksBack){
          weekBack <- validWeeksBack[k]
          colname <- paste0("weekBackDensity", weekBack)
          summaryColumn <- paste0("dailyDensitySU", j-weekBack*7)
          joined[which(matchIds), (colname) :=
                   summary[[summaryColumn]][matchPlaceIds]
                 ]
        }
      }
    }
    
    
    # Add interaction between x and y
    joined[,xy:=x*y]
    
    # Add kernel density estimates? Doesn't seem to make much sense given the
    # large number of distributions
    joined[,xZ:=abs(x-medX)/(relaxedMadX)]
    joined[,xZSlightRelax:=abs(x-medX)/(slightlyRelaxedMadX)]
    joined[,xZMadAcGroup2:=abs(x-medXAcGroup2)/(0.1 + madXAcGroup2)]
    joined[,xZLog:=log(abs(xZ) + addConstantZ)]
    joined[,xZLog2:=xZLog^2]
    
    joined[,yZ:=abs(y-medY)/(relaxedMadY)]
    joined[,yZSlightRelax:=abs(y-medY)/(slightlyRelaxedMadY)]
    joined[,yZMadAcGroup2:=abs(y-medYAcGroup2)/(0.05 + madYAcGroup2)]
    joined[,yZLog:=log(abs(yZ) + addConstantZ)]
    joined[,yZLog2:=yZLog^2]
    
    joined[,madRatio:=madX/madY]
    joined[,relaxedMadRatio:=relaxedMadX/relaxedMadY]
    joined[,slightlyRelaxedMadRatio:=slightlyRelaxedMadX/slightlyRelaxedMadY]
    joined[,logRelaxedMadRatio:=log(relaxedMadRatio)]
    joined[,logSlightlyRelaxedMadRatio:=log(slightlyRelaxedMadRatio)]
    
    # Add edge features
    joined[,distanceXBorder := 5-abs(5-x)]
    joined[,distanceYBorder := 5-abs(5-y)]
    joined[,distanceXLowBorder := x]
    joined[,distanceXHighBorder := 10-x]
    joined[,distanceYLowBorder := y]
    joined[,distanceYHighBorder := 10-y]
    
    # Optionally exclude records from joined that have a true place id which
    # is not present in the summary period
    # Clarification: dropping of new places
    if(excludeNewPlaces && blockTarget != "test"){
      validBatches <- joined$actual_place_id %in% rawSummaryPlaces
      # table(validBatches)
      joined <- joined[which(validBatches)]
    }
    
    #####################
    # Top M calculation #
    #####################
    if(topM < topN || addTopMProbs){
      predictorData <- joined[, topMModels[[1]]$predictors, with=FALSE]
      predictorData <- as.matrix(predictorData)
      # predictorDataM <- as.matrix(predictorData)
      joined[,predict := 0]
      
      # Calculate mean place probability and the probability of all top M
      # models
      for(j in 1:nbTopMModels){
        colname <- paste0("topMProb",j)
        val <- predict(topMModels[[j]]$model, predictorData, missing = NA)
        joined[, (colname) := val]
        joined[, predict := predict + get(colname)]
        gc()
        
        # Extract the predicted probabilities in separate variables if they
        # are used to calculate the combined predicted place probabilities
        if(combineTopMLR){
          assign(colname, val)
        }
      }
      
      # Overwrite the predict column if the top M model probabilities are not
      # averaged but combined by a LR model
      if(combineTopMLR){
        
        probPredict <-
          (do.call(cbind,lapply(1:nbTopMModels,
                                function(x) get(paste0("topMProb",x)))) %*%
             topMLRModel$coefficients[-1])
        joined[, predict:=probPredict]
      }
      
      # Order probability and restrict to the topM for each observation
      joined[,order_predict := match(1:length(predict),
                                     order(-predict)), by=row_id]
      joined <- joined[order_predict <= topM,]
      
      # Copy the predicted order and probability to the top M rank
      joined[, topMRank := order_predict]
      joined[, topMPredict := predict/nbTopMModels]
      
      # Optionally remove top M probability columns
      if(!addTopMProbs){
        joined <- joined[,!grepl("^topMProb", names(joined)), with=FALSE]
      }
      
      # Remove topM calculation columns
      joined[,predict:=NULL]
      joined[,order_predict:=NULL]
    }
    
    # Drop columns that are not of interest for further steps
    joined <- joined[,!grepl("^countAcGroup|^relAcGroup|^relRecAcGroup|^medXAcGroup|^madXAcGroup|^medYAcGroup|^madYAcGroup",
                             names(joined)), with=FALSE]
    
    # Create the target save folder if it does not yet exist
    dir.create(saveDir, showWarnings = FALSE)
    
    # Write the joined data to the target save file
    saveRDS(joined, savePath)
  }
}