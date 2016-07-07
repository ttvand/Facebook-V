# Logic to create accuracy summary features

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(ggplot2)

# Target date in case of generating the summary features for the train data
targetDate <- "23-05-2016"

# Summary type (for training or testing)
summaryType <- c("train","test")[1]

# Days considered before the end of the summary period
daysConsidered <- 10

# Set the data path to the summary data
if(summaryType=="train"){
  dataPath <- paste0("../Downsampling/",targetDate,"/summary.rds")
} else{
  dataPath <- paste0("../Data/train.rds")
}

# Set the path to the summary data
summaryPath <- file.path(getwd(), targetDate,
                         paste0(summaryType, " summary features.rds"))

# Read in the summary data
placeSummary <- readRDS(summaryPath)

# Read in the target data
raw <- readRDS(dataPath)

# Only consider the last daysConsidered to calculate the MAD versus ac group
raw <- raw[time > (max(time) - daysConsidered*1440)]

# Add the accuracy groups
raw[,accuracyGroup3 := cut(accuracy, breaks=c(0, 45, 85, 1e5))]
raw[,accuracyGroup32 := cut(accuracy,
                            breaks=c(0, 6, 10, 15, 20, 26, 31, 35, 40, 46,
                                     51, 55, 57, 59, 61, 63, 64, 65, 66, 68,
                                     69, 71, 73, 76, 81, 96, 125, 157, 165,
                                     173, 223, 424, 1e5))]

# Calculate the distance to the location center
raw[,matchIds := match(place_id, placeSummary$place_id)]
raw <- raw[!is.na(matchIds),] # Places with a count less than 3 (so 1 or 2)
raw[,xVar := abs(x-placeSummary[matchIds,medX])]
raw[,yVar := abs(y-placeSummary[matchIds,medY])]


#####################################
# 3 group accuracy summary features #
#####################################

# Calculate the 3 accuracy group summary features
accuracyGroupSummary <- raw[,.(relativeSize = .N/nrow(raw),
                                madX = median(xVar),
                               madY = median(yVar)),
                            by = accuracyGroup3]
accuracyGroupSummary <- accuracyGroupSummary[order(accuracyGroup3)]
accuracyGroupSummary$ID <- 1:nrow(accuracyGroupSummary)
accuracyGroupSummary$lowCut <- 
  as.numeric(gsub('^\\(|,.*','',levels(raw$accuracyGroup3)))
accuracyGroupSummary$lowCut[1] <- 0

# Write the time trend summary to the target date folder
folderPath <- file.path(getwd(), targetDate)
dir.create(folderPath, showWarnings = FALSE)
fn <- paste(summaryType, "accuracy summary features 3.rds")
saveRDS(accuracyGroupSummary, file.path(folderPath, fn))


######################################
# 32 group accuracy summary features #
######################################

# Calculate the 32 accuracy group summary features
accuracyGroupSummary <- raw[,.(relativeSize = .N/nrow(raw),
                               madX = median(xVar),
                               madY = median(yVar)),
                            by = accuracyGroup32]
accuracyGroupSummary <- accuracyGroupSummary[order(accuracyGroup32)]
accuracyGroupSummary$ID <- 1:nrow(accuracyGroupSummary)
accuracyGroupSummary$lowCut <- 
  as.numeric(gsub('^\\(|,.*','',levels(raw$accuracyGroup32)))
accuracyGroupSummary$lowCut[1] <- 0

# Write the time trend summary to the target date folder
folderPath <- file.path(getwd(), targetDate)
dir.create(folderPath, showWarnings = FALSE)
fn <- paste(summaryType, "accuracy summary features 32.rds")
saveRDS(accuracyGroupSummary, file.path(folderPath, fn))