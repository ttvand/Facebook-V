# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)
library(viridis)

# Read a subset of the train data
dataFn <- "trainSubset.rds"
dataSubset <- readRDS(dataFn)

# Order by place id
setkey(dataSubset, place_id)

# Extract the unique place ids for subsetting in the app graphs
# placeIds <- unique(dataSubset$place_id)
placeIds <- table(dataSubset$place_id)
placeIds <- names(sort(placeIds, decreasing = TRUE))

# List of analysis variables
analysisVars <- c("accuracy", "x", "y", "time", "hour", "day")
analysisVarsComp <- c(analysisVars, "logAccuracy")

# Add the hour and day of week to the raw data
dataSubset[,hour := floor((time %% (60*24))/60)]
dataSubset[,day := floor((time %% (60*24*7)) / (60*24))]

# Add the log transformed accuracy
dataSubset[,logAccuracy := log(accuracy)]

# Add the accuracy groups
dataSubset[,accuracyGroup := cut(accuracy, breaks=c(0,45,85,1e5))]

# Text to display in the about section
aboutString <- "This app was developed by Tom Van de Wiele and relates to the exploratory analysis of the 'Facebook V: Predicting Check Ins' competition <br/><br/>"