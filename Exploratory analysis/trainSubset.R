# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Data")

# Load the required libraries
library(data.table)

# Number of considered random records
nbRandomRecords <- 5e3


#######################################################################
# Take a random subset of the train data - used for Shiny exploratory #
# analysis                                                            #
#######################################################################

train <- readRDS("train.rds")

# Select 5,000 random records and remove all place_ids that don't belong to
# the selected place ids
studyTrainIds <- sample(1:nrow(train), nbRandomRecords)
studiedPlaces <- unique(train[studyTrainIds, place_id])
studiedPlaceData <- train[place_id %in% studiedPlaces,]

# Convert the place id to character
studiedPlaceData[,place_id := as.character(place_id)]

# Write the subset to a file
saveRDS(studiedPlaceData, "trainSubset.rds")