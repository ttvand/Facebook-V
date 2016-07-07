# Neural network first level learner
# Learn and save a model given
#   - The train batch path
#   - The train features
#   - The considered top L places for each modeled check-in
#   - The learning hyperparameters

# H2O download reference: http://www.h2o.ai/download/h2o/r

hyperParStringNN <- function(hyperpar, topL){
  paste(hyperpar$activation,
        paste(hyperpar$epochs, "epochs"),
        paste(hyperpar$input_dropout_ratio, "inputS"),
        paste0(ifelse(hyperpar$balance_classes, "", "not ") , "balanced"),
        paste(length(hyperpar$hidden), "hiddenL"), sep = " - ")
}

generateNnH2O <- function(trainId, loadFromExternalHD, predictors, topL,
                          hyperpar){
  
  # Path to store the generated model
  saveDir <- file.path(getwd(), "NnH2O", "Models")
  savePath <- file.path(saveDir, paste0("nnH2O - ",
                                        paste("train", trainId), " - ",
                                        hyperParStringNN(hyperpar, topL),
                                        ".rds"))
  if(!file.exists(savePath)){
    
    # Set up the path to the train features
    if(loadFromExternalHD){
      featuresDir <- "H:/Facebook features"
    } else{
      featuresDir <- paste0("../../Feature engineering/", "23-05-2016", "/",
                            "train")
    }
    featuresPath <- file.path(featuresDir,
                              paste0("train features - Random batch - ",
                                     trainId, ".rds"))
    
    # Set the considered target variable
    targetVar <- "placeMatch"
    
    # Read in the features
    features <- readRDS(featuresPath)
    
    # Set the top M rank if it is not available in the train features
    # Warnings suppressed because not all row ids have 100 places
    if(!"topMRank" %in% names(features)){
      suppressWarnings(features[, topMRank := 1:100, by=row_id])
    }
    
    # Subset on the top L places (based on top M rank)
    features <- features[topMRank <= topL]
    
    # Extract predictors data and labels from the features data
    predictorData <- features[, predictors, with=FALSE]
    labels <- features[, targetVar, with=FALSE]
    labels[[targetVar]] <- labels[, 1, with=FALSE][[1]] == 1
    
    # Convert the predictor data to a binary H2O format
    predictorDataH <- as.h2o(cbind(labels, predictorData))
    
    # Learn the model
    model <- 
      h2o.deeplearning(x = 1 + (1:length(predictors)),  # Predictor cols
                       y = 1, # Label column
                       training_frame = predictorDataH, # data in H2O format
                       activation = hyperpar$activation,
                       input_dropout_ratio = hyperpar$input_dropout_ratio,
                       hidden_dropout_ratios = hyperpar$hidden_dropout_ratios,
                       balance_classes = hyperpar$balance_classes,
                       hidden = hyperpar$hidden,
                       epochs = hyperpar$epochs)
    
    # Write the model to the target save file
    saveRDS(list(model=model, predictors=predictors, hyperpar=hyperpar),
            savePath)
  }
}

#############
# Test case #
#############

# Load the required libraries
library(h2o)
library(data.table)

# Initialize local H2O instance, assigned to run on all available CPUs
localH2O <- h2o.init(nthreads = -1,
                     max_mem_size = "15g")
h2o.removeAll()

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")

# Read in the ordered features
features <- readRDS("orderedFeatures.rds")
features <- setdiff(features, "regionDensity")

trainId <- 26
topL <- 2
hyperpar <- list(activation = "RectifierWithDropout",
                 input_dropout_ratio = 0.5,
                 hidden_dropout_ratios = c(0.5,0.5,0.5),
                 balance_classes = TRUE,
                 hidden = c(150,100,50),
                 epochs = 100
                 )
generateNnH2O(trainId, TRUE, features, topL, hyperpar)
