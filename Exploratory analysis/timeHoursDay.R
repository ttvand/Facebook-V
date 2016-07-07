# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Exploratory analysis")

# Load the required libraries
library(data.table)
library(bit64)
library(MASS)
library(EnvStats)
library(ggplot2)

# Target date
targetDate <- "23-05-2016"

# Set seed in order to obtain reproducible results
set.seed(14)

# Maybe even zoom in further than hour?

# Hour and week offset in minutes
dayHourOffset <- 120

# Option to look at the hourly and daily trends
inspectHourly <- FALSE
inspectDaily <- FALSE
inspectWeekly <- TRUE
processHourly <- FALSE
processDaily <- FALSE

# Gaussian smoothing of the hourly period (period must be odd!)
hourSmooth <- 5
hourDiffs <- (-(hourSmooth-1)/2):((hourSmooth-1)/2)
extremeSdHour <- 2
hourDens <- dnorm(hourDiffs/((hourSmooth-1)/2)*extremeSdHour)
hourDens <- hourDens / sum(hourDens) # Gaussian smoother
# hourDens <- rep(1/hourSmooth, hourSmooth) # Uniform smoother

# Gaussian smoothing of the weekly period (period must be odd!)
daySmooth <- 3
dayDiffs <- (-(daySmooth-1)/2):((daySmooth-1)/2)
extremeSdDay <- 2
dayDens <- dnorm(dayDiffs/((daySmooth-1)/2)*extremeSdDay)
dayDens <- dayDens / sum(dayDens) # Gaussian smoother
# dayDens <- rep(1/weekSmooth, weekSmooth) # Uniform smoother

# Number of studied top places and ranking of first studied place
studyAllPlaces <- FALSE
firstPlaceRank <- 80000
nbTopPlaces <- 10

# Maximum considered sample size
maxSample <- 5e5

# Train - Validation cutoff time
trainTimeLimit <- 580000


################################################################"

# Read in the training data
train <- readRDS("../Data/train.rds")

# Load summary table of the train data by place_id
summaryFilePath <- paste0("../Feature engineering/", targetDate,
                          "/test summary features.rds")
placeSummary <- readRDS(summaryFilePath)

# Study specific place ids to get a better idea of the true variation
# Random sampling so that larger places have higher occurence probs
topPlaces <- placeSummary[order(-count),place_id]
if(!studyAllPlaces){
  studyTrainIds <- sample(1:nrow(train), maxSample/200)
  topPlaces <- unique(train[studyTrainIds, place_id])
}
topPlaceData <- train[place_id %in% topPlaces,]

# Set key
setkey(topPlaceData, place_id)

# Add hour, day week and time part (train/validation)
topPlaceData[,hour := floor(((time + dayHourOffset) %% 1440)/60)]
topPlaceData[,day := floor(((time + dayHourOffset) %% (1440*7)) / 1440)]
topPlaceData[,week := floor(time/(1440*7))]
topPlaceData[,timeHalf := ifelse(time<trainTimeLimit,"Train","Validation")]

# Inspect hourly trends potentially by day of the week or time period
# (train vs validation)
if(inspectHourly){
  wrap <- TRUE
  for(i in 1:nbTopPlaces){
    p <- ggplot(topPlaceData[place_id==topPlaces[i]], aes(x=hour)) +
      xlim(0,23) +
      geom_bar()
    if(wrap){
      p <- p +
        # facet_wrap(~day)
        facet_wrap(~timeHalf,nrow=2)
    }
    p <- p +
      ggtitle(paste("Top place",i))
    print(p)
    cat("Press [enter] to continue")
    line <- readline()
  }
}

# Inspect daily trends
if(inspectDaily){
  for(i in 1:nbTopPlaces){
    p <- ggplot(topPlaceData[place_id==topPlaces[i]], aes(x=day)) +
      xlim(0,6) +
      geom_bar()
    p <- p +
      ggtitle(paste("Top place",i))
    print(p)
    cat("Press [enter] to continue")
    line <- readline()
  }
}

# Inspect weekly trends
if(inspectWeekly){
  for(i in 1:(nbTopPlaces*3)){
    p <- ggplot(topPlaceData[place_id==topPlaces[i]], aes(x=week)) +
      xlim(0,80) +
      geom_bar()
    p <- p +
      ggtitle(paste("Top place",i))
    print(p)
    cat("Press [enter] to continue")
    line <- readline()
  }
}


############################################
# Preparation for validation measures      #
# of hourly, daily and longitudinal models #
############################################

# Split the data into a train and a test set
trainIds <- topPlaceData$time < trainTimeLimit

# Add the training counts
trainCounts <- topPlaceData[trainIds, .N, by=place_id]

# Drop training ids that have no observations in the training period
trainIds <- trainIds & topPlaceData$place_id %in% trainCounts$place_id

# Add training counts
topPlaceData <- topPlaceData[trainCounts]

#######################################
# Analysis section 1: hourly analysis #
#######################################

# Model the hourly probability as a function of
#   - The historical hourly fraction
#   - The historical five-hour fraction
#   - The number of historical observations
# Initial simple approach: ML estimation
# Prob = [(hourly_fraction + c1*five_hour_fraction)/(1+c1)*count + k/24]/
#         (count+k)
# With c1 and k>=0
# => Estimate c and k
# For now it is assumed that the hourly probability does not vary over time
if(processHourly){
  # Add hourly fractions to the top places data table
  for(i in 0:23){
    # Progress message
    cat("Processing mean hour",i,"\n")
    
    colname <- paste0("hourMean",i)
    hourMeans <- topPlaceData[trainIds, mean(hour==i), by=place_id]
    setkey(hourMeans, "place_id")
    
    # Drop unmatched hour means
    hourMeans <- hourMeans[place_id %in% trainCounts$place_id]
    
    # Replace NA's by 1/24 (no observations in training period)
    if(any(is.na(hourMeans$V1))) browser()
    # hourMeans[is.na(V1),V1:=1/24]
    
    # Rename to colname
    setnames(hourMeans,"V1",colname)
    
    # Merge with top place data
    topPlaceData <- topPlaceData[hourMeans]
  }
  
  # Add smoothed hourly fractions to the top places data table
  for(i in 0:23){
    # Progress message
    cat("Processing smoothed hour",i,"\n")
    
    smoothHourSum <- 0
    for(j in 1:hourSmooth){
      hourDiff <- hourDiffs[j]
      hour <- (i+hourDiff) %% 24
      colname <- paste0("hourMean",hour)
      smoothHourSum <- smoothHourSum + hourDens[j]*
        topPlaceData[,colname, with=FALSE][[1]]
    }
    colname <- paste0("hourMeanS",i)
    topPlaceData[,(colname) := smoothHourSum]
  }
  
  # Likelihood estimation of the validation part 
  valLikHour <- function(params, data){
    c1 <- exp(params[1])
    k <- exp(params[2])
    # k2 <- exp(params[3])
    
    # Extract the matrix ids of interest
    nbPoints <- nrow(data)
    hourIds <- (1:nbPoints) + (3+data[,2])*nbPoints
    smoothHourIds <- (1:nbPoints) + (27+data[,2])*nbPoints
    
    # Prob = [(hourly_fraction + c1*five_hour_fraction)/(1+c1)*count + k/24]/
    #         (count+k)
    count <- data[,3]
    probs <- ((data[hourIds] + c1*data[smoothHourIds])/(1+c1)*count + k/24)/
      (count+k)
    # probs <- ((data[hourIds] +
    #              c1*data[smoothHourIds]*k1/(10+count))/(1+c1*k1/(10+count))*
    #             count + k2/24)/
    #   (count+k2)
    -mean(log(probs))
    # -mean(probs)
  }
  
  # Convert data to a matrix for faster subsetting (variable column for
  # different rows)
  topPlaceDataM <-
    matrix(as.numeric(as.matrix(topPlaceData[,-c(1:4,6,8:9),with=FALSE])),
           nrow=nrow(topPlaceData))
  
  # Maximize the likelihood for a maximum size sample (computation time)
  init <- c(0,0)
  testIds <- topPlaceData$time >= trainTimeLimit &
    topPlaceData$place_id %in% trainCounts$place_id
  testData <- topPlaceDataM[testIds,]
  
  if(nrow(testData)>maxSample){
    testData <- testData[sample(1:nrow(testData), maxSample),]
  }
  mlParams <- optim(init, valLikHour, data=testData)
  cat("Optimal c1:", round(exp(mlParams$par[1]),3),
      "; optimal k1:", round(exp(mlParams$par[2]),3),
      "; optimal k2:", round(exp(mlParams$par[3]),3),"\n")
  cat("Mean information gain hour in day:", round(24/exp(mlParams$value), 3))
}


######################################
# Analysis section 2: daily analysis #
######################################

# Add daily fractions to the top places data table
if(processDaily){
  for(i in 0:6){
    # Progress message
    cat("Processing mean day",i,"\n")
    
    colname <- paste0("dayMean",i)
    dayMeans <- topPlaceData[trainIds, mean(day==i), by=place_id]
    setkey(dayMeans, "place_id")
    
    # Drop unmatched day means
    dayMeans <- dayMeans[place_id %in% trainCounts$place_id]
    
    # Rename to colname
    setnames(dayMeans,"V1",colname)
    
    # Merge with top place data
    topPlaceData <- topPlaceData[dayMeans]
  }
  
  # Add smoothed daily fractions to the top places data table
  for(i in 0:6){
    # Progress message
    cat("Processing smoothed day",i,"\n")
    
    smoothDaySum <- 0
    for(j in 1:daySmooth){
      dayDiff <- dayDiffs[j]
      day <- (i+dayDiff) %% 7
      colname <- paste0("dayMean",day)
      smoothDaySum <- smoothDaySum + dayDens[j]*
        topPlaceData[,colname, with=FALSE][[1]]
    }
    colname <- paste0("dayMeanS",i)
    topPlaceData[,(colname) := smoothDaySum]
  }
  
  # Likelihood estimation of the validation part 
  valLikDay <- function(params, data){
    c1 <- exp(params[1])
    k <- exp(params[2])
    # k2 <- exp(params[3])
    
    # Extract the matrix ids of interest
    nbPoints <- nrow(data)
    dayIds <- (1:nbPoints) + (3+data[,2])*nbPoints
    smoothDayIds <- (1:nbPoints) + (10+data[,2])*nbPoints
    
    # Prob = [(daily_fraction + c1*three_day_fraction)/(1+c1)*count + k/7]/
    #         (count+k)
    count <- data[,3]
    probs <- ((data[dayIds] + c1*data[smoothDayIds])/(1+c1)*count + k/7)/
      (count+k)
    -mean(log(probs))
    # -mean(probs)
  }
  
  # Convert data to a matrix for faster subsetting (variable column for
  # different rows)
  topPlaceDataM <-
    matrix(as.numeric(as.matrix(topPlaceData[,-c(1:4,6:7,9),with=FALSE])),
           nrow=nrow(topPlaceData))
  
  # Maximize the likelihood for a maximum size sample (computation time)
  init <- c(0,0)
  testIds <- topPlaceData$time >= trainTimeLimit &
    topPlaceData$place_id %in% trainCounts$place_id
  testData <- topPlaceDataM[testIds,]
  
  if(nrow(testData)>maxSample){
    testData <- testData[sample(1:nrow(testData), maxSample),]
  }
  mlParams <- optim(init, valLikDay, data=testData)
  cat("Optimal c1:", round(exp(mlParams$par[1]),3),
      "; optimal k1:", round(exp(mlParams$par[2]),3),
      "; optimal k2:", round(exp(mlParams$par[3]),3),"\n")
  cat("Mean information gain day of week:", round(7/exp(mlParams$value), 3))
}

############################################
# Weekly probability is a different koekie #
# Start simple!                            #
############################################