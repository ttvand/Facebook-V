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
distribType <- c("Time","Random")[1]
dataTypes <- c("train","validation")
typeId <- 2
dataType <- dataTypes[typeId]
valBatch <- 72
raw <- readRDS(file.path(getwd(),targetFolder,
                         paste0(dataType," - ",distribType," batch - ",
                                valBatch,".rds")))

cat("Time range:",range(raw$time),"\n\n")

# Extract the other (train vs validation batch) and make sure that the ids do not
# have a union
otherDataType <- dataTypes[3-typeId]
otherRaw <- readRDS(file.path(getwd(),targetFolder,
                         paste0(otherDataType," - ",distribType," batch - ",
                                valBatch,".rds")))
cat("Intersect count train validation:",
    length(intersect(raw$row_id,otherRaw$row_id)))