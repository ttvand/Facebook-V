# Logic to create region density summary features

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(ggplot2)
library(viridis)
library(plotly)
library(spatstat)

# Target date in case of generating the summary features for the train data
targetDate <- "23-05-2016"

# Summary type (for training or testing)
summaryType <- c("train","test")[2]

# Set the number of X and Y blocks
nbXBlocks <- 100
nbYBlocks <- 400
nbBlocks <- nbXBlocks*nbYBlocks

# Days considered before the end of the summary period
trainDaysConsidered <- round((786239-587158)/1440)

# Set the data path to the raw data
dataPath <- paste0("../Data/", summaryType, ".rds")

# Read in the raw data
raw <- readRDS(dataPath)

# Only consider the last daysConsidered for the train data
if(summaryType=="train"){
  raw <- raw[time > (max(time) - trainDaysConsidered*1440)]
}

# Calculate the block densities
regionDensities <- data.table(xMin = rep((0:(nbXBlocks-1))*10/nbXBlocks,
                                         each = nbYBlocks),
                              xMax = rep((1:nbXBlocks)*10/nbXBlocks,
                                         each = nbYBlocks),
                              yMin = rep((0:(nbYBlocks-1))*10/nbYBlocks,
                                         nbXBlocks),
                              yMax = rep((1:nbYBlocks)*10/nbYBlocks,
                                            nbXBlocks),
                              density = 0)
regionDensities$blockId <- 1:nrow(regionDensities)
raw[x<1e-6,x:=1e-6]
raw[y<1e-6,y:=1e-6]
raw[x==10,x:=10-1e-6]
raw[y==10,y:=10-1e-6]
raw[,blockId := 1 + floor((y-1e-7)*nbYBlocks/10) +
      nbYBlocks*floor((x-1e-7)*nbXBlocks/10)]
regionDensities$density <- table(raw$blockId)*nbBlocks/nrow(raw)

# Smooth the densities based on the 2D grid
smoothedDens <- as.matrix(blur(as.im(matrix(regionDensities$density,
                                            nrow=nbYBlocks)),
                               sigma = 0.8))
smoothedDens <- smoothedDens/mean(smoothedDens)
regionDensities$smoothDensity <- as.vector(smoothedDens)

# Visualize densities
ggplot(raw, aes(x = x, y = y, z = accuracy)) +
  stat_summary_2d(fun = length, bins = 50) +
  scale_fill_viridis()

# Write the region density summary to the target date folder
folderPath <- file.path(getwd(), targetDate)
dir.create(folderPath, showWarnings = FALSE)
fn <- paste(summaryType, "region densities.rds")
saveRDS(regionDensities, file.path(folderPath, fn))