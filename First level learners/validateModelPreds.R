# Logic that validates the base model performances

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")

# Load the required libraries
library(data.table)
library(bit64)
library(ggplot2)

# Validation range: 21:30
# Processed ids
ids <- 1:4 #21:30
nbIds <- length(ids)

# External path to load predictions from and store to
loadStoreExternalHD <- TRUE
externalPredictionsFolder <- "H:/First level combined predictions"

# Last covariate before the prediction columns
lastCovariate <- "slightlyRelaxedMadRatio"

# Validation summary
summary <- NULL

# Loop over the validation ids and add the accuracy and mape scores to the
# summary
for(i in 1:nbIds){
  # Extract the validation id
  id <- ids[i]
  
  # Load the validation predictions
  if(loadStoreExternalHD){
    predictionsFolder <- externalPredictionsFolder
  } else{
    predictionsFolder <- file.path(getwd(), "Combined predictions")
  }
  
  # Extract the predictions path
  batchExtension <- paste0("validation features - Random batch - ", id,
                           ".rds")
  predictionsPath <- file.path(predictionsFolder, batchExtension)
  predictions <- readRDS(predictionsPath)
  
  # Extract the prediction columns on the first run
  if(i == 1){
    predictionColumns <-
      names(predictions)[-(1:which(names(predictions) == lastCovariate))]
    nbPredictionColumns <- length(predictionColumns)
  }
  
  # Loop over all the prediction columns and add the summary features
  for(j in 1:nbPredictionColumns){
    # Show progress message
    cat("Processing validation batch", id, "and prediction", j, "of",
        nbPredictionColumns, "\n")
    
    # Extract the predicted orders
    predictCol <- predictionColumns[j]
    predictions[,batch_predict_o := match(1:length(get(predictCol)),
                                          order(-get(predictCol))), by=row_id]
    
    # What is the order of the predicted ids?
    orderPredict <- predictions[placeMatch==TRUE, batch_predict_o]
    batchSize <- length(unique(predictions[,row_id]))
    
    # Calculate the % correct
    fracCorrect <- sum(orderPredict == 1)/batchSize
    
    # Calculate MAP@3
    map3 <- sum((orderPredict==1) + (orderPredict==2)/2 +
                  (orderPredict==3)/3)/batchSize
    
    # Calculate the average accuracy
    avAc <- sum(predictions[placeMatch==TRUE, get(predictCol)])/batchSize
    
    # Extract the number of features and the top L of the predictions
    featureParts <- unlist(strsplit(predictCol, split = "-"))
    nbFeatures <- as.numeric(substr(featureParts[3], 1, 3))
    etaC <- as.numeric(substr(featureParts[6], 1, 2))
    topL <- as.numeric(substring(featureParts[4], 2))
    
    # Append the batch summary
    summary <- rbind(summary,
                     data.frame(validationId = id,
                                fracCorrect = fracCorrect,
                                map3 = map3,
                                avAc = avAc,
                                nbFeatures = nbFeatures,
                                etaC = etaC,
                                topL = topL))
  }
}

# Generate validation summary plots
p <- ggplot(summary, aes(x = nbFeatures, y=map3, color = factor(topL))) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 210)

pB <- ggplot(summary, aes(x = factor(nbFeatures), y=map3,
                          fill = factor(topL))) +
  geom_boxplot()

q <- ggplot(summary, aes(x = factor(etaC), y=map3, fill = factor(etaC))) +
  geom_boxplot()

r <- ggplot(summary, aes(x = factor(topL), y=map3, fill = factor(topL))) +
  geom_boxplot()

plot(p)
plot(pB)
plot(q)
plot(r)