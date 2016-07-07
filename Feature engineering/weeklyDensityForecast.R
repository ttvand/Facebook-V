# Logic to generate weekly density forecasts using various time series models
# Auto.arima from forecast
# Exponential smoothing from forecast
# Holt from forecast

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)
library(doParallel)
library(forecast)

# Summary type (for training or testing)
summaryType <- c("train","test")[2]

# Target date
targetDate <- "23-05-2016"

# Register cluster for parallel execution
nbClusters <- 3
cl <- makePSOCKcluster(nbClusters)
registerDoParallel(cores=cl)


#######################################################################

# Load the weekly density data
fn <- paste(summaryType, "weekly densities.rds")
weeklySummary <- readRDS(fn)

# Set the predict horizon
if(summaryType == "train"){
  predictRange <- 59:77
} else{
  predictRange <- 78:100
}
nbPredicted <- length(predictRange)

# Convert the weekly densities to matrix format
weeklyDensitiesM <- t(as.matrix(weeklySummary[,-1, with=FALSE]))

# Calculate the number of series
nbSeries <- nrow(weeklySummary)
# predictions[1,] <- weeklyDensitiesM[nrow(weeklyDensitiesM),]

# Generate time series predictions in parallel
tsPredictions <- 
  foreach(i=1:min(c(Inf,nbSeries)), .packages = c('data.table', 'forecast',
                                                  'bit64')
  ) %dopar% {
    # for(i in 1:min(c(1e3,nbSeries))){
    # # Show progress message
    # if(i %% 100 == 0){
    #   cat("Progress:", round(i/nbSeries*100,2), "%\n")
    # }
    
    # Extract the observations and fit the models
    observations <- weeklyDensitiesM[,i]
    arimaFit <- auto.arima(observations, approximation = TRUE)
    out <- c(as.numeric(forecast(arimaFit, h=nbPredicted)$mean),
             as.numeric(ses(observations, h=nbPredicted)$mean),
             as.numeric(holt(observations, h=nbPredicted)$mean)
    )
    
    out
  }

# Convert the predictions to a matrix
predictionsM <- t(do.call(rbind, tsPredictions))

# Split the matrix up into the three generated predictions
arimaPredictions <-
  rbind(weeklyDensitiesM[nrow(weeklyDensitiesM),][1:ncol(predictionsM)],
        predictionsM[1:nbPredicted,])
sesPredictions <-
  rbind(weeklyDensitiesM[nrow(weeklyDensitiesM),][1:ncol(predictionsM)],
        predictionsM[nbPredicted + (1:nbPredicted),])
holtPredictions <-
  rbind(weeklyDensitiesM[nrow(weeklyDensitiesM),][1:ncol(predictionsM)],
        predictionsM[2*nbPredicted + (1:nbPredicted),])

# Combine the summary file of the predictions
predictionsSummary <- list(arimaPredictions = arimaPredictions,
                           sesPredictions = sesPredictions,
                           holtPredictions = holtPredictions)

# Save the predictions to the predictions file
fn <- paste(summaryType, "weekly predictions.rds")
saveRDS(predictionsSummary, file.path(getwd(), fn))

# Timing message
cat("Execution finished @", as.character(Sys.time()), "\n\n")

# Stop the cluster and remove zombie processes
stopCluster(cl)
closeAllConnections()
