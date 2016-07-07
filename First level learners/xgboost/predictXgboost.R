# xgboost first level prediction function

predictXgboost <- function(features, trainModel, featuresPath=NULL){
  # Load the features data
  if(is.null(features) || !is.data.frame(features)){
    if(!is.null(featuresPath)){
      features <- readRDS(featuresPath)
    } else{
      features <- readRDS(features)
    }
  }
  
  # Extract predictors data from the features data
  predictorData <- features[, trainModel$predictors, with=FALSE]
  
  # Convert the predictor data to a matrix
  predictorDataM <- as.matrix(predictorData)
  
  # Output the prediction
  predict(trainModel$model, predictorDataM, missing = NA)
}