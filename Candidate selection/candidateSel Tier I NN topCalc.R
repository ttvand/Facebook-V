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
# Train range: 11:219 since 1-10 in candidate selection
# Validation range: 1:73
# Test range: 1:287
ids <- 178:180  #11:287 #1:287
nbIds <- length(ids)

# In what data set should neighbors be considered
blockTarget <- c("train", "validation", "test")[3]
targetDataExtension <- ifelse(blockTarget=="test", "test", "train")

# How should distances be rescaled? Don't consider time at this point!!
distanceConstantsX <- c(1,2.5,4,5.5,7,12,30)[4] #[c(1,2,5,7)] #[2]
nbDistanceConstants <- length(distanceConstantsX)

# Register cluster for parallel execution
nbClusters <- 3
cl <- makePSOCKcluster(nbClusters)
registerDoParallel(cores=cl)

# What top N should be considered?
topN <- ifelse(blockTarget=="test", 100, 100)

# Top N method - 1 based on manual tuning, 2 based on best LR model
# Both show comparable performance wrt to top 100 but drastic difference wrt
# top M << 100 where method 2 is better than method 1
topNMethod <- ifelse(blockTarget=="test", 1, 1)

# Ks in NN calculation
Ks <- c(2500,1000,500,250,100,50,20,10,5,1) # - Preferred for now
# Ks <- c(5000,2500,1000,500,250,100)
# Ks <- c(5000,1000,500,100)

# Source the nearest neighbors function
source("getTopKNNDT.R")


#############################################################################

# Create the target folder if it does not exist yet
dir.create(file.path(getwd(), targetDate, targetDataExtension),
           showWarnings = FALSE)

# Calculate the processed ids for which to calculate the top N nearest
# neighbors
for(i in 1:nbIds){
  # Extract the processed id
  id <- ids[i]
  
  # Loop over the distance constants
  for(j in 1:nbDistanceConstants){
    distanceConstantX <- distanceConstantsX[j]
    distanceConstants <- c(distanceConstantX,1)^2 # c(12,1)^2 #c(18,1,Inf)^2
    
    # Display progress message
    cat("Processing", blockTarget, "id", id, "with distance constant",
        distanceConstantX, "@", as.character(Sys.time()), "\n")
    
    # Path to the target top N file
    savePath <- file.path(getwd(), targetDate, targetDataExtension,
                          paste0("Top ", blockTarget," NN ",
                                 sqrt(distanceConstants[1]),
                                 " - Random batch - ", id, ".rds"))
    
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
      if(topNMethod==1){
        topNBatch <- getTopKNNDT1(batch, Ks, distanceConstants, topN,
                                  paste0(targetDate, "/", targetDataExtension))
      } else{
        topNBatch <- getTopKNNDT2(batch, distanceConstants, topN,
                                  paste0(targetDate, "/", targetDataExtension))
      }
      
      # Check for missed matches in the KNN calculation
      if(max(topNBatch[,1 + topN*(1+2*length(Ks))]) > 1e10) browser()
      
      # Check the frequency of the actual class in the top N if blockTarget is
      # not test
      if(blockTarget!="test"){
        topNContained <- (rowSums(topNBatch[,1:topN]==
                                    batch$place_id)>0)
        
        # Display the frequency of the actual class in the top N
        cat("Mean top ",topN," accuracy % : ",
            round(mean(topNContained, na.rm = TRUE)*100,2),"\n\n",sep = "")
      }
      
      # Store the top N calculation for the considered batch
      saveRDS(topNBatch, savePath)
    }
  }
}

# Timing message
cat("Execution finished @", as.character(Sys.time()), "\n\n")

# Stop the cluster and remove zombie processes
stopCluster(cl)
closeAllConnections()