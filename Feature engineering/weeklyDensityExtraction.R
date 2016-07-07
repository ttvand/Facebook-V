# Logic to create weekly density summary features

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Summary type (for training or testing)
summaryType <- c("train","test")[2]

# Target date
targetDate <- "23-05-2016"

# Set the data path to the raw data
if(summaryType=="train"){
  dataPath <- paste0("../Downsampling/",targetDate,"/summary.rds")
} else{
  dataPath <- "../Data/train.rds"
}

# Read in the raw data
raw <- readRDS(dataPath)

# Add the week column
raw$week <- floor(raw$time / (7*1440))

# Add weekly density columns
summary <- raw[,.N,by=place_id]
summary[,N:=NULL]
setkey(summary, place_id)
weekRange <- sort(unique(raw$week))
for(i in 1:length(weekRange)){
  consideredWeek <- weekRange[1+length(weekRange)-i]
  
  # Progress message
  cat("Processing week", consideredWeek, "\n")
  
  # Calculate weekly summary
  weekSummary <- raw[week==consideredWeek, list(density = .N), by=place_id]
  weekSummary$density <- weekSummary$density/sum(weekSummary$density)*1e5
  colName <- paste0("week",consideredWeek,"Density")
  setnames(weekSummary, "density", colName)
  
  # Merge with summary
  setkey(weekSummary, place_id)
  summary <- weekSummary[summary]
  summary[is.na(summary[,get(colName)]), colName := 0, with=FALSE]
}

# Write the region density summary to the target date folder
folderPath <- file.path(getwd())
dir.create(folderPath, showWarnings = FALSE)
fn <- paste(summaryType, "weekly densities.rds")
saveRDS(summary, file.path(folderPath, fn))