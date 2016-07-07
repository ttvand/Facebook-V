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

# Hour and week offset in minutes
dayHourOffset <- 0

# Sample size
sampleSize <- NA #1e7


################################################################"

# Read in the training data
train <- readRDS("../Data/train.rds")

# Subsample train data
if(!is.na(sampleSize)){
  train <- train[sample(1:nrow(train), sampleSize)]
}

# Order train data by time
train <- train[order(time)]

# Load summary table of the train data by place_id
summaryFilePath <- paste0("../Feature engineering/", targetDate,
                          "/test summary features.rds")
placeSummary <- readRDS(summaryFilePath)

# Add day and week from time
train[,totalDay := floor((time + dayHourOffset)/(1440))]
train[,week := floor(time/(1440*7))]


############################################################
# Study variation from the location center versus accuracy #
############################################################

# Calculate the distance to the location center
train[,matchIds := match(place_id, placeSummary$place_id)]
train[,count := placeSummary[matchIds,count]]
train[,xVar := abs(x-placeSummary[matchIds,medX])]
train[,yVar := abs(y-placeSummary[matchIds,medY])]

# Drop places with less than 20 observations
train <- train[count>=20,]


###############################################################
# Plot the variation from the center versus the mean accuracy #
###############################################################

# Calculate accuracy bins
train[,accuracyGroup := cut_number(accuracy, 30)]
# train[,accuracyGroup := cut(accuracy, breaks=c(0,45,85,1e5))]

# Calculate time bins
train[,timeGroup := cut_number(time, 6)]

# # Boxplot of distance from center versus accuracy group
# p <- ggplot(train[xVar<1/4,], aes(x=xVar)) +
#   geom_density() +
#   facet_wrap(~accuracyGroup, ncol=1)
# print(p)

# Calculate summary statistics for the different accuracy groups
accSummary <- train[, list(.N,
                           meanXVar = mean(xVar),
                           medianXVar = median(xVar),
                           meanYVar = mean(yVar),
                           medianYVar = median(yVar)),
                    by=accuracyGroup]
accSummary <- accSummary[order(accuracyGroup),]

# Plot accuracy summary statistics
xVarMax <- max(accSummary$medianXVar[1:(nrow(accSummary)-1)])
p <- ggplot(accSummary, aes(x=accuracyGroup, y=medianXVar)) +
  geom_point() +
  ylim(c(NA,xVarMax))
print(p)

yVarMax <- max(accSummary$medianYVar[1:(nrow(accSummary)-1)])
q <- ggplot(accSummary, aes(x=accuracyGroup, y=medianYVar)) +
  geom_point() +
  ylim(c(NA,yVarMax))
print(q)

# Calculate the time summary
accTimeSummary <- train[, list(.N, as.numeric(median(accuracy))),
                        by=list(accuracyGroup, timeGroup)]

# Calculate summary statistics for the different accuracy groups
accTimeSummary <- train[, list(meanXVar = mean(xVar),
                           medianXVar = median(xVar),
                           meanYVar = mean(yVar),
                           medianYVar = median(yVar),
                           count = length(x)),
                    by=list(accuracyGroup, timeGroup)]
accTimeSummary <- accTimeSummary[order(accuracyGroup),]

# Plot accuracy summary statistics
r <- ggplot(accTimeSummary, aes(x=accuracyGroup, y=meanXVar,
                                shape=timeGroup, col=timeGroup)) +
  geom_point() + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
  # facet_wrap(~timeGroup, ncol=1)
print(r)
