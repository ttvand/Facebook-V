# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Data")

# Load the required libraries
library(data.table)

# Target date
targetDate <- "23-05-2016"

############################################################################
# Split the data into equally spaced but overlapping blocks.               #
# The splitted data is used in the KNN calculation                         #
############################################################################

# Generate blocks for the validation or for the test set prediction? 
blockTarget <- c("train","test")[1]

# Calculate the name of the target main folder and create it
dir.create(file.path(getwd(), targetDate), showWarnings = FALSE)

# Set the desired blocks sizes
# The number of blocks has to be distinct for different X distance constants
# This is an unhidden feature but was grown historically.
# The block sizes must align with the block sizes in getTopKNNDT!
distanceConstantX <- 2.5
distanceId <- which(distanceConstantX==c(1, 2.5, 4, 7, 12, 30))
xBlockSize <- c(0.2,0.25,0.5,0.4,0.5,1)[distanceId] #0.2 #1 #2
nbXBlocks <- 10/xBlockSize - 1
yBlockSize <- c(0.2,0.1,0.1,0.05,0.04,0.025)[distanceId] #0.2 #0.04 #0.05 #0.1 #0.4 #1 
nbYBlocks <- 10/yBlockSize - 1
nbBlocks <- nbXBlocks*nbYBlocks

# Load the data
if(blockTarget=="train"){
  train <- readRDS(paste0("../Downsampling/",targetDate,"/summary.rds"))
} else{
  train <- readRDS("train.rds")
}

# Create the target folder if it does not exist yet
targetFolder <- file.path(getwd(), targetDate, blockTarget)
dir.create(targetFolder, showWarnings = FALSE)

# # Write block sizes to the target folder
# saveRDS(list(xBlockSize = xBlockSize, yBlockSize = yBlockSize),
#         file.path(targetFolder, "blockSizes.rds"))

# Generate blocks
for(blockId in 1:nbBlocks){
  # Progress message
  cat("Processing block",blockId,"of",nbBlocks,"\n")
  
  # Subset data
  xStart <- xBlockSize * ((blockId-1) %% nbXBlocks)
  xMin <- xStart-(xStart==0)
  xMax <- xStart + 2*xBlockSize + 1e-6
  yStart <- floor((blockId-1)/nbXBlocks)*yBlockSize
  yMin <- yStart-(yStart==0)
  yMax <- yStart + 2*yBlockSize + 1e-6
  block <- train[x>xMin & x<=xMax & y>yMin & y<=yMax]
  
  # Compose save path
  savePath <- file.path(targetFolder,
                        paste0(nbBlocks," - ", xStart, " - ", yStart, ".rds"))
  
  # Save block
  saveRDS(block, savePath)
}