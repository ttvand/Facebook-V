# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Downsampling")

# Load the required libraries
library(data.table)
library(bit64)

# Calculate the name of the targetFolder
targetFolder <- "23-05-2016"


##############################################################################

# Read in the raw data
valBatch <- 200
raw <- readRDS(file.path(getwd(), targetFolder,
                         paste0("test - Random batch - ", valBatch,".rds")))

cat("Batch size:", nrow(raw), "\n")
cat("Time range:", range(raw$time), "\n\n")