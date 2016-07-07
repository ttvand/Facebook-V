# Logic that serves as a friendly interface to generate the base first level
# learners

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")

# Load the required libraries
library(data.table)
library(xgboost)
library(nnet)

# Load the train features from the external HD?
loadFromExternalHD <- FALSE

# List model types that ignore predictors with missing values
dropMissingModels <- c("nnet")

# Execution time range in 24 hour notation (option to start fit at certain hour)
timeRange <- c(0,24)

# Source the base learner functions
source(file.path(getwd(), "xgboost", "generateXgboost.R"))
source(file.path(getwd(), "nnet", "generateNnet.R"))

# Wait for a valid time range
while(TRUE){
  exit <- as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
    as.numeric(format(Sys.time(),"%H")) <= timeRange[2]
  
  if(exit){
    break
  }
  
  # Display sleep message
  cat("Waiting for a valid time range", timeRange, "@",
      as.character(Sys.time()), "\n")
  
  # Sleep since the features file is not available
  Sys.sleep(300) 
}

# Set the list of the learners that need to be generated
learnerList <- list()
ids <- 171:200
for(i in 1:length(ids)){
  nbPred <- sample(180:240, 1)
  etaC <- sample(11:12, 1)
  learnerList <-
    c(learnerList,
      list(list(type = "xgboost", trainId = ids[i], nbPredictors = nbPred,
                topL = 20,
                hyperpar = list(nrounds = 500, etaC = etaC, subsample = 0.5,
                                colsample_bytree = 0.6, max.depth = 11))))
}

# ids <- 72:90
# for(i in 1:length(ids)){
#   nbPred <- 430
#   etaC <- sample(11:12, 1)
#   learnerList <-
#     c(learnerList,
#       list(list(type = "xgboost", trainId = ids[i], nbPredictors = nbPred,
#                 topL = 20,
#                 hyperpar = list(nrounds = 500, etaC = etaC, subsample = 0.5,
#                                 colsample_bytree = 0.6, max.depth = 11))))
# }

# ids <- 92:110
# for(i in 1:length(ids)){
#   nbPred <- sample(100:285, 1)
#   etaC <- sample(11:12, 1)
#   learnerList <-
#     c(learnerList,
#       list(list(type = "xgboost", trainId = ids[i], nbPredictors = nbPred,
#                 topL = 100,
#                 hyperpar = list(nrounds = 500, etaC = etaC, subsample = 0.5,
#                                 colsample_bytree = 0.6, max.depth = 11))))
# }
# 
# ids <- 111:130
# for(i in 1:length(ids)){
#   nbPred <- 430
#   etaC <- sample(11:12, 1)
#   learnerList <-
#     c(learnerList,
#       list(list(type = "xgboost", trainId = ids[i], nbPredictors = nbPred,
#                 topL = 100,
#                 hyperpar = list(nrounds = 500, etaC = etaC, subsample = 0.5,
#                                 colsample_bytree = 0.6, max.depth = 11))))
# }

# learnerList <-
#   c(learnerList,
#     list(list(type = "nnet", trainId = 42, nbPredictors = 142, topL = 20,
#               hyperpar = list(size = 20, decay = 0.3, maxit = 2e3))))

# Count the number of base learners
nbBaseLearners <- length(learnerList)

# Load the ordered features and ordered features that show no missing values
predictors <- readRDS("orderedFeatures.rds")
predictorsNoMissing <- readRDS("orderedFeaturesNoMissing.rds")
missingPredictors <- setdiff(predictors, predictorsNoMissing)

for(i in 1:nbBaseLearners){
  # Extract the considered base learner
  learner <- learnerList[[i]]
  learnerType <- learner$type
  
  # Progress message
  cat("\nGenerating learner", i, "of", nbBaseLearners, "@",
      as.character(Sys.time()), "\n")
  cat(learnerType, "- id", learner$trainId, "-", learner$nbPredictors,
      "pred -", "topL", learner$topL, "\n\n")
  
  # Extract the considered predictors - some models handle missing values and
  # some don't
  consideredPredictors <- predictors[1:learner$nbPredictors]
  if(learnerType %in% dropMissingModels){
    consideredPredictors <-
      consideredPredictors[!consideredPredictors %in% missingPredictors]
  }
  
  # Find the name of the generation function of the considered learner
  generationFunction <- paste0("generate",
                               toupper(substr(learnerType, 1, 1)),
                               substring(learnerType, 2))
  
  # Generate the learner
  do.call(generationFunction, list(trainId = learner$trainId,
                                   loadFromExternalHD = loadFromExternalHD,
                                   predictors = consideredPredictors,
                                   topL = learner$topL,
                                   hyperpar = learner$hyperpar))
}