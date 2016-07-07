# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Candidate selection")

# Load the required libraries
library(data.table)
library(bit64)
library(doParallel)
library(fastOrdeR)

# Target date
targetDate <- "23-05-2016"

# Overwrite target file if it exists?
overwrite <- FALSE

# Processed ids
# Train range: 31:150
# Validation range: 1:30
# Test range: 1:287
ids <- c(180:182, 200)
nbIds <- length(ids)

# In what data set should neighbors be considered
blockTarget <- c("train", "validation", "test")[1]
targetDataExtension <- ifelse(blockTarget=="test", "test", "train")

# What distance constants should be considered
distanceConstants <- c(0.4, 0.8, 1.3, 2, 3.2, 4.5, 6, 8)^2

# Register cluster for parallel execution
nbClusters <- 3
cl <- makePSOCKcluster(nbClusters)
registerDoParallel(cores=cl)

# Ks in NN calculation
Ks <- c(375, 175, 100, 75, 35)

# What top N should be considered?
topN <- 100

# Source the mid nearest neighbors function
source("midKnnCalc.R")


#############################################################################

# Create the target folder if it does not exist yet
dir.create(file.path(getwd(), targetDate, targetDataExtension),
           showWarnings = FALSE)

# Calculate the processed ids for which to calculate the top N nearest
# neighbors
for(i in 1:nbIds){
  # Extract the processed id
  id <- ids[i]
  
  # Display progress message
  cat("Processing", blockTarget, "id", id, "@", as.character(Sys.time()),
      "\n")
  
  # Path to the target top N file
  savePath <- file.path(getwd(), targetDate, targetDataExtension,
                        paste0("Top ", blockTarget,
                               " mid NN - Random batch - ", id, ".rds"))
  
  # Extract the nearest neighbors if the file does not exist or overwrite = T
  if(overwrite || !file.exists(savePath)){
    # Load the considered batch
    filePath <- paste0("../Downsampling/", targetDate, "/",
                       blockTarget, " - Random batch - ",
                       id, ".rds")
    batch <- readRDS(filePath)
    # batch <- batch[x>9 & y>9,]
    # batch <- batch[x<1 & y<1,]
    
    # Calculate the nearest neighbors for the considered train batch
    topNBatch <- midKnnCalc(batch, Ks, distanceConstants, topN,
                            paste0(targetDate, "/", targetDataExtension))
    
    # Store the top N calculation for the considered batch
    saveRDS(topNBatch, savePath)
  }
}

# Timing message
cat("Execution finished @", as.character(Sys.time()), "\n\n")

# Stop the cluster and remove zombie processes
stopCluster(cl)
closeAllConnections()
