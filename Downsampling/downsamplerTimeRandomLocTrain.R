# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Downsampling")


############################################################################
# Train data is not split in groups - used for feature generation, not for #
# model building!                                                          #
# Downsampling logic to build the validation data                          #
############################################################################

# Load the required libraries
library(data.table)
library(bit64)

# Set the seed in order to obtain reproducible results
targetSeed <- 14
set.seed(targetSeed)

# Maximum sample size of the time and random blocks
maxSample <- 30000

# What fraction of the train data is left for pure validation
validationFraction <- 0.25

# What part of the data is used to construct the place summary statistics
# The ratio test/train is about 0.31 so 0.7 seems like a good pick
summaryFraction <- 0.7

# Set the desired blocks sizes
xBlockSize <- 2 #1 #2
nbXBlocks <- 10/xBlockSize - 1
yBlockSize <- 0.2 #0.4 #1
nbYBlocks <- 10/yBlockSize - 1
nbBlocks <- nbXBlocks*nbYBlocks

# Calculate the name of the targetFolder
targetFolder <- format(Sys.time(),"%d-%m-%Y")


##############################################################################

# Read in the raw data
raw <- readRDS(paste0("../Data/train.rds"))

# Order the raw data by time
raw <- raw[order(time)]

# Calculate the number of batches and the according batch sizes
nbRecords <- nrow(raw)

# Randomize such that the train and validation set cover disjoint periods
nbSummaryRecords <- round(nbRecords*summaryFraction)
nonSummaryIds <- (nbSummaryRecords+1):nbRecords
nbIds <- length(nonSummaryIds)
nbValidationIds <- round(nbIds*validationFraction)
nbTrainIds <- nbIds-nbValidationIds

##########################################################################
# Calculate the six group ids - train and validation blocks are mutually #
# disjoint!!                                                             #
##########################################################################
# Six groups:
#   1) Random training
#   2) Fixed x-y training rectangles
#   3) Fixed time training periods
#   4) Random validation - random in place and time
#   5) Fixed x-y validation
#   6) Fixed time validation

summary <- raw[1:(nonSummaryIds[1]-1)]
randomIds <- sample(1:nbIds)
trainIds <- nonSummaryIds[randomIds[1:nbTrainIds]]
validationIds <- nonSummaryIds[randomIds[-(1:nbTrainIds)]]
train <- raw[trainIds]
validation <- raw[validationIds]

# Calculate random and time training ids
nbTrainBatches <- ceiling(nbTrainIds/maxSample)
trainBatchSizes <- rep(floor(nbTrainIds/nbTrainBatches),nbTrainBatches)
itemsLeft <- nbTrainIds-sum(trainBatchSizes)
if(itemsLeft>0){
  trainBatchSizes[1:itemsLeft] <- trainBatchSizes[1:itemsLeft] + 1
}

# Calculate ids for groups 1 and 3 (random and time train) as well as 
# 6 (time validation)
randomTrainingIds <- vector(mode = "list", length=nbTrainBatches)
timeTrainingIds <- vector(mode = "list", length=nbTrainBatches)
timeValidationIds <- vector(mode = "list", length=nbTrainBatches)
timeOrder <- order(train$time)
nextId <- 1
minTime <- -Inf
for(i in 1:nbTrainBatches){
  batchSize <- trainBatchSizes[i]
  randomTrainingIds[[i]] <- trainIds[nextId:(nextId+batchSize-1)]
  timeTrainingIds[[i]] <- trainIds[timeOrder[nextId:(nextId+batchSize-1)]]
  nextId <- nextId + batchSize
  
  maxTime <- max(raw[timeTrainingIds[[i]],time])+100*(i==nbTrainBatches)
  timeValidationIds[[i]] <- validationIds[validation$time > minTime &
                                            validation$time <= maxTime]
  minTime <- maxTime
}

# Calculate random validation ids
nbValidationBatches <- ceiling(nbValidationIds/maxSample)
validationBatchSizes <- rep(floor(nbValidationIds/nbValidationBatches),
                            nbValidationBatches)
itemsLeft <- nbValidationIds-sum(validationBatchSizes)
if(itemsLeft>0){
  validationBatchSizes[1:itemsLeft] <- validationBatchSizes[1:itemsLeft] + 1
}

# Calculate ids for group 4 (random validation)
randomValidationIds <- vector(mode = "list", length=nbValidationBatches)
nextId <- 1
for(i in 1:nbValidationBatches){
  batchSize <- validationBatchSizes[i]
  randomValidationIds[[i]] <- validationIds[nextId:(nextId+batchSize-1)]
  nextId <- nextId + batchSize
}

# Calculate ids for groups 2 and 5 (block train and validation)
blockTrainingIds <- vector(mode = "list", length=nbBlocks)
blockValidationIds <- vector(mode = "list", length=nbBlocks)
for(i in 1:nbBlocks){
  xStart <- xBlockSize * ((i-1) %% nbXBlocks)
  xMin <- xStart-(xStart==0)
  xMax <- xStart + 2*xBlockSize + 1e-6
  yStart <- floor((i-1)/nbXBlocks)*yBlockSize
  yMin <- yStart-(yStart==0)
  yMax <- yStart + 2*yBlockSize + 1e-6
  blockTrainingIds[[i]] <- trainIds[train$x>xMin & train$x<=xMax &
                                      train$y>yMin & train$y<=yMax]
  blockValidationIds[[i]] <- validationIds[validation$x>xMin &
                                             validation$x<=xMax &
                                             validation$y>yMin &
                                             validation$y<=yMax]
}

# Combine all the ids
combinedIds <- c(randomTrainingIds,blockTrainingIds,timeTrainingIds,
                 randomValidationIds,blockValidationIds,timeValidationIds)
lengthsGroups <- c(length(randomTrainingIds),
                   length(blockTrainingIds),
                   length(timeTrainingIds),
                   length(randomValidationIds),
                   length(blockValidationIds),
                   length(timeValidationIds))
lengthsCumsum <- cumsum(lengthsGroups)
namesHeaders <- c("train - Random batch - ",
                  paste0("train - Block ",nbBlocks," "),
                  "train - Time batch - ",
                  "validation - Random batch - ",
                  paste0("validation - Block ",nbBlocks," "),
                  "validation - Time batch - ")

# Calculate the names for the downsampled batches
nbBatchesTotal <- length(combinedIds)
batchNames <- rep("",nbBatchesTotal)
startIds <- c(1,lengthsCumsum[-6]+1)
for(i in c(1,3,4,6)){
  ids <- startIds[i]:lengthsCumsum[i]
  nbIds <- length(ids)
  batchNames[ids] <- paste0(namesHeaders[i],1:nbIds)
}

# Set the names for the x-y blocks
for(i in 1:nbBlocks){
  xStart <- xBlockSize * ((i-1) %% nbXBlocks)
  yStart <- floor((i-1)/nbXBlocks)*yBlockSize
  batchNames[i+lengthsCumsum[1]] <- paste0(namesHeaders[2], xStart," ", yStart)
  batchNames[i+lengthsCumsum[4]] <- paste0(namesHeaders[5], xStart," ", yStart)
}

##############
# Save logic #
##############

# Save the downsampled batches as well as the information used to generate
# them
dir.create(file.path(getwd(), targetFolder), showWarnings = FALSE)

lengthsGroups <- c(length(randomTrainingIds),
                   length(blockTrainingIds),
                   length(timeTrainingIds),
                   length(randomValidationIds),
                   length(blockValidationIds),
                   length(timeValidationIds))

# Save the information used to generate the subsamples
creationInfo <- list(targetSeed=targetSeed,
                     maxSample=maxSample,
                     summaryFraction=summaryFraction,
                     validationFraction=validationFraction,
                     xBlockSize=xBlockSize,
                     nbXBlocks=nbXBlocks,
                     yBlockSize=yBlockSize,
                     nbYBlocks=nbYBlocks,
                     targetFolder=targetFolder,
                     nbTrainGroupsRandom=length(randomTrainingIds),
                     nbTrainGroupsTime=length(timeTrainingIds),
                     nbValidationGroupsRandom=length(randomValidationIds),
                     nbValidationGroupsTime=length(timeValidationIds),
                     randomTrainingSizes=sapply(randomTrainingIds,length),
                     blockTrainingSizes=sapply(blockTrainingIds,length),
                     timeTrainingSizes=sapply(timeTrainingIds,length),
                     randomValidationSizes=sapply(randomValidationIds,length),
                     blockValidationSizes=sapply(blockValidationIds,length),
                     timeValidationSizes=sapply(timeValidationIds,length),
                     batchNames=batchNames
)
saveRDS(creationInfo, file=file.path(getwd(), targetFolder,
                                     paste0("train ",
                                            "CreationInfo.rds")))

# Save the summary data as a whole
saveRDS(summary, file=file.path(getwd(), targetFolder,"summary.rds"))

# Write all the batches to the target folder
for(i in 1:nbBatchesTotal){
  # Progress message
  cat("Processing subset",i,"of",nbBatchesTotal,"\n")
  
  # Extract the target row ids
  targetRowIds <- combinedIds[[i]]
  
  # Subset the data
  subset <- raw[targetRowIds]
  
  # Save the subset
  saveRDS(subset, file=file.path(getwd(), targetFolder,
                                 paste0(batchNames[i],".rds")))
}