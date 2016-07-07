# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Data")

# Load the required libraries
library(data.table)
library(bit64)

# Read the raw data
train <- fread("train.csv")
test <- fread("test.csv")

# Write the raw data to rds format
saveRDS(train, file.path(getwd(), "train.rds"))
saveRDS(test, file.path(getwd(),"test.rds"))