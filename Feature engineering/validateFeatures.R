# Logic to check that specific features are calculated correctly

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Target date
targetDate <- "23-05-2016"

# Load the train features
trainFeatures <- readRDS("train features.rds")

# Load the raw data
rawPath <- paste0("../Downsampling/",targetDate,"/summary.rds")
raw <- readRDS(rawPath)

# Load the raw batch
filePath <- paste0("../Downsampling/", targetDate, "/",
                   "train", " - Random batch - ",
                   16, ".rds")
batch <- readRDS(filePath)

# Load the raw KNN data
topNOtherPath <- paste0("../Candidate selection/", targetDate, "/",
                        "train", "/Top ", "train",
                        " NN ", 1,
                        " - Random batch - ", 16, ".rds")
topNBatch <- readRDS(topNOtherPath)


# Check the neighbor distance feature by inspecting the most extreme 
# NN distances
range(topNBatch[,2110]) # Batch range
# 1) Most isolated observation in the batch
maxDistanceId <- which.max(trainFeatures$neighborDistance20)
maxDistanceRowId <- trainFeatures$row_id[maxDistanceId]
topNMaxDistance <- t(topNBatch[which(batch$row_id==maxDistanceRowId),])
rawBatchMaxDistance <- batch[row_id==maxDistanceRowId,]
mostIsolated <- c(trainFeatures$x[maxDistanceId],
                  trainFeatures$y[maxDistanceId])

# Calculate the distance to the most isolated point and compare to what it 
# should be (reference distance)
refDistance <- as.numeric(topNBatch[which(batch$row_id==maxDistanceRowId),2110])
raw[,distanceMI := 1e10*
      (((x-mostIsolated[1])^2) + (y-mostIsolated[2])^2)]
raw <- raw[order(distanceMI)]
View(raw[1:1e3])
View(trainFeatures[row_id==trainFeatures[maxDistanceId,row_id], -(10:80),
                   with=FALSE])


# 2) Least isolated observation in the batch
minDistanceId <- which.min(trainFeatures$neighborDistance20)
minDistanceRowId <- trainFeatures$row_id[minDistanceId]
topNMinDistance <- t(topNBatch[which(batch$row_id==minDistanceRowId),])
rawBatchMinDistance <- batch[row_id==minDistanceRowId,]
leastIsolated <- c(trainFeatures$x[minDistanceId],
                    trainFeatures$y[minDistanceId])

# Calculate the distance to the most isolated point and compare to what it 
# should be (reference distance)
refDistance <- as.numeric(topNBatch[which(batch$row_id==minDistanceRowId),2110])
raw[,distanceMI := 1e10*
      (((x-leastIsolated[1])^2) + (y-leastIsolated[2])^2)]
raw <- raw[order(distanceMI)]
View(raw[1:1e3])
View(trainFeatures[row_id==trainFeatures[minDistanceId,row_id], -(10:80),
                   with=FALSE])
