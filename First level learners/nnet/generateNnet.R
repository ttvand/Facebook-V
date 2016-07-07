# Neural network first level learner using the nnet package
# Learn and save a model given
#   - The train batch path
#   - The train features
#   - The considered top L places for each modeled check-in
#   - The learning hyperparameters

hyperParStringNN <- function(hyperpar, topL){
  paste(paste("size", hyperpar$size),
        paste("decay", hyperpar$decay),
        paste("maxit", hyperpar$maxit),
        paste0("L", topL), sep = " - ")
}

generateNnet <- function(trainId, loadFromExternalHD, predictors, topL,
                         hyperpar){

  # Path to store the generated model
  saveDir <- file.path(getwd(), "nnet", "Models")
  savePath <- file.path(saveDir, paste0("nnet - ",
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

    # Drop features that are not used
    features <-
      features[,names(features) %in% c(predictors, targetVar, "topMRank"),
               with=FALSE]

    # Set the top M rank if it is not available in the train features
    # Warnings suppressed because not all row ids have 100 places
    if(!"topMRank" %in% names(features)){
      suppressWarnings(features[, topMRank := 1:100, by=row_id])
    }

    # Subset on the top L places (based on top M rank)
    features <- features[topMRank <= topL]

    # Normalize the predictors data
    predictorMeans <- sapply(features[, predictors, with=FALSE], mean)
    predictorSds <- sapply(features[, predictors, with=FALSE], sd)
    targetCol <- which(names(features) == targetVar)
    featuresS <- data.table(scale(features))

    # Drop variables that are not predictors or the target variable
    modelData <- cbind(features[, targetVar, with=FALSE],
                       featuresS[, predictors, with=FALSE])

    # Set up the NN model formula
    form <- paste(targetVar, paste(predictors, collapse=" + "), sep=" ~ ")

    # Learn the model
    model <- nnet(as.formula(form),
                  size   = hyperpar$size,
                  maxit  = hyperpar$maxit,
                  decay  = hyperpar$decay,
                  data   = modelData,
                  linout = TRUE,
                  trace  = TRUE,
                  MaxNWts = 1e4)

    # model <- train(as.formula(form),
    #                data   = modelData,
    #                method = 'nnet',
    #                size   = hyperpar$size,
    #                maxit  = hyperpar$maxit,
    #                decay  = hyperpar$decay,
    #                linout = FALSE,
    #                trace  = TRUE)

    # Clean environment
    rm(list=setdiff(ls(), c("model", "predictors", "hyperpar", "predictorMeans",
                            "predictorSds", "savePath")))

    # Write the model to the target save file
    saveRDS(list(model=model, predictors=predictors, hyperpar=hyperpar,
                 predictorMeans=predictorMeans, predictorSds=predictorSds),
            savePath)
  }
}

# #############
# # Test case #
# #############
# 
# # Load the required libraries
# library(nnet)
# library(data.table)
# 
# # Set working directory
# setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")
# 
# # Read in the ordered features
# features <- readRDS("orderedFeaturesNoMissing.rds")
# 
# trainId <- 44
# topL <- 20
# hyperpar <- list(size = 20,
#                  decay = 0.3,
#                  maxit = 2000
# )
# generateNnet(trainId, TRUE, features, topL, hyperpar)