# Logic to check that the feature range is similar for train and test data

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)

# Load the train and test features
train <- readRDS("train features.rds")
test <- readRDS("test features.rds")

# Drop training specific columns from train
trainSpecificCols <- setdiff(names(train),names(test))
train <- train[, -trainSpecificCols, with=FALSE]

# Reorder the test columns so that they match the train columns
# test <- test[, names(train), with=FALSE]
which(names(train) != names(test))

# Mean analysis
trainMeans <- sapply(train, mean, na.rm=TRUE)
testMeans <- sapply(test, mean, na.rm=TRUE)

# Inspect most extreme ratio differences
meanRatio <- sort(trainMeans/testMeans)
plot(meanRatio)
head(meanRatio)
tail(meanRatio)

# Median analysis
trainMedians <- sapply(train, median, na.rm=TRUE)
testMedians <- sapply(test, median, na.rm=TRUE)

# Inspect most extreme ratio differences
medianRatio <- sort(trainMedians/testMedians)
plot(medianRatio)
head(medianRatio)
tail(medianRatio)
