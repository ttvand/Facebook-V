# Logic to create weekly trend features

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Target date in case of generating the summary features for the train data
targetDate <- "23-05-2016"

# Hour and week offset in minutes
dayHourOffset <- 0

# Set the data path to the summary data (always use all data for time trends)
dataPathTrain <- paste0("../Data/train.rds")
dataPathTest <- paste0("../Data/test.rds")

# Read in the summary data
train <- readRDS(dataPathTrain)
test <- readRDS(dataPathTest)
summary <- rbind(train[,place_id:=NULL], test)

# Convert time to week and calculate the weekly fraction of checkins
summary$week <- floor((summary$time + dayHourOffset)/(1440*7))
weekTrends <- summary[,list(N = length(x)*1440*7/(max(time)-min(time)),
                            medAc = as.numeric(median(accuracy)),
                            meanAc = mean(accuracy),
                            madAc = mad(accuracy),
                            meanLogAc = mean(log(1+accuracy))),
                      by=week]
weekTrends <- weekTrends[order(week)]

# Write the time trend summary to the target date folder
folderPath <- file.path(getwd(), targetDate)
dir.create(folderPath, showWarnings = FALSE)
fn <- paste0("weekly summary features.rds")
saveRDS(weekTrends, file.path(folderPath, fn))