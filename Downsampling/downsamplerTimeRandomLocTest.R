# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Downsampling")


########################################
# Downsampling logic for the test data #
# Split raw data in random blocks      #
########################################

# Load the required libraries
library(data.table)
library(bit64)

# Set the seed in order to obtain reproducible results
targetSeed <- 14
set.seed(targetSeed)

# Maximum sample size of the time and random blocks
maxSample <- 30000

# Calculate the name of the targetFolder
targetFolder <- "23-05-2016"


##############################################################################

# Read in the raw data
raw <- readRDS(paste0("../Data/test.rds"))

# Order the raw data by time
raw <- raw[order(time)]

# Calculate the number of batches and the according batch sizes
nbRecords <- nrow(raw)
randomIds <- sample(1:nbRecords)

# Calculate batch sizes
nbBatches <- ceiling(nbRecords/maxSample)
batchSizes <- rep(floor(nbRecords/nbBatches), nbBatches)
itemsLeft <- nbRecords - sum(batchSizes)
if(itemsLeft>0){
  batchSizes[1:itemsLeft] <- batchSizes[1:itemsLeft] + 1
}

# Calculate random test ids
randomTestIds <- vector(mode = "list", length=nbBatches)
nextId <- 1
for(i in 1:nbBatches){
  batchSize <- batchSizes[i]
  randomTestIds[[i]] <- randomIds[nextId:(nextId+batchSize-1)]
  nextId <- nextId + batchSize
}

# Calculate the names for the downsampled batches
batchNames <- paste("test - Random batch -", 1:nbBatches)

##############
# Save logic #
##############

# Save the downsampled batches as well as the information used to generate
# them
dir.create(file.path(getwd(), targetFolder), showWarnings = FALSE)

# Save the information used to generate the subsamples
creationInfo <- list(targetSeed = targetSeed,
                     maxSample = maxSample,
                     targetFolder = targetFolder,
                     nbBatches = nbBatches,
                     batchSizes = batchSizes,
                     batchNames = batchNames
)
saveRDS(creationInfo, file=file.path(getwd(), targetFolder,
                                     paste0("train ",
                                            "CreationInfo.rds")))

# Write all the batches to the target folder
for(i in 1:nbBatches){
  # Progress message
  cat("Processing subset",i,"of",nbBatches,"\n")
  
  # Extract the target row ids
  targetRowIds <- randomTestIds[[i]]
  
  # Subset the data
  subset <- raw[targetRowIds]
  
  # Save the subset
  saveRDS(subset, file=file.path(getwd(), targetFolder,
                                 paste0(batchNames[i], ".rds")))
}