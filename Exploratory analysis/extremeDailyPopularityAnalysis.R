# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Exploratory analysis")

# Load the required libraries
library(data.table)
library(bit64)
library(ggplot2)
library(plotly)
library(viridis)
library(ggvis)

# Target date
targetDate <- "23-05-2016"

# Do not set seed in order to avoid reproducible results
# set.seed(14)

# Hour and week offset in minutes
dayHourOffset <- 120

# Number of studied top places and ranking of first studied place
firstStudiedPlace <- 1
nbStudiedPlaces <- 100

# Train - Validation cutoff time
trainTimeLimit <- 587000


################################################################"

# Read in the training data and order by time
train <- readRDS("../Data/train.rds")
test <- readRDS("../Data/test.rds")
train <- train[order(time)]

# Load summary table of the train data by place_id
summaryFilePath <- paste0("../Feature engineering/", targetDate,
                          "/test summary features.rds")
placeSummary <- readRDS(summaryFilePath)

# Add hour, day week and time part (train/validation)
train[,hour := floor(((time + dayHourOffset) %% 1440)/60)]
train[,totalHour := floor((time + dayHourOffset)/(60))]
train[,day := floor(((time + dayHourOffset) %% (1440*7)) / 1440)]
train[,totalDay := floor((time + dayHourOffset)/(1440))]
train[,week := floor(time/(1440*7))]
train[,timeHalf := ifelse(time<trainTimeLimit,"Train","Validation")]
train[,oddWeek := ifelse(week %% 2 == 0,"Even","Odd")]


#######################################################################
# Study specific place ids to get a better idea of the true variation #
# Random sampling so that larger places have higher occurence probs   #
#######################################################################

# Look at the daily frequencies for all places
dailyFreqs <- train[,.N, by=list(place_id, totalDay)]
dailyFreqs <- dailyFreqs[order(-N),]

# Extract studied places
studiedPlaces <- unique(dailyFreqs[1:(nbStudiedPlaces*2), place_id])[
  1:nbStudiedPlaces]
studiedPlaceData <- train[place_id %in% studiedPlaces,]

# Daily trend of top places
for(i in firstStudiedPlace:nbStudiedPlaces){
  placeData <- studiedPlaceData[place_id==studiedPlaces[i]]
  daySumPlace <-  placeData[,.N, by=totalDay]
  maxFreqDay <- daySumPlace[which.max(N),totalDay]
  p <- ggplot(placeData, aes(x=totalDay)) +
    xlim(0,550) +
    geom_bar() +
    geom_vline(xintercept = maxFreqDay - 364, colour="green") +
    geom_vline(xintercept = maxFreqDay - 371, colour="blue") +
    geom_vline(xintercept = maxFreqDay + 364, colour="green") +
    geom_vline(xintercept = maxFreqDay + 371, colour="blue")
  medianX <- train[place_id==studiedPlaces[i],median(x)]
  medianY <- train[place_id==studiedPlaces[i],median(y)]
  p <- p +
    ggtitle(paste("Studied place",i,":",studiedPlaces[i], "[",
                  medianX, ",", medianY, "]"))
  print(p)
  cat("Press [enter] to continue")
  line <- readline()
}