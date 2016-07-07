# NNet first level prediction function

predictNnet <- function(features, trainModel, featuresPath=NULL){
  # Load the features data
  if(is.null(features) || !is.data.frame(features)){
    if(!is.null(featuresPath)){
      features <- readRDS(featuresPath)
    } else{
      features <- readRDS(features)
    }
  }
  
  # Extract predictors data afrom the features data
  predictorData <- features[, trainModel$predictors, with=FALSE]
  
  # Convert the predictor data to a matrix
  predictorDataM <- as.matrix(predictorData)
  
  # Normalize the validation predictors
  predictorDataMS <- scale(predictorDataM,
                           center = trainModel$predictorMeans,
                           scale = trainModel$predictorSds)
  
  # Output the prediction
  predict(trainModel$model, predictorDataMS)
}