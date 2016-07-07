# Logic to create and store a data table with summary features of the studied
# data period

# Time adjusted median accuracy and MAD accuracy
# Day of week and time of day features that also incorporate the extremeness of
# the trends
# TODO Activity one year back - In the combination file of the place features 
# with new observations
# TODO Extrapolated place popularity (week by week) - Difficult! Look for less
# obvious patterns

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Load the required libraries
library(data.table)
library(bit64)

# Summary type (for training or testing)
summaryType <- c("train","test")[2]

# Target date in case of generating the summary features for the train data
targetDate <- "23-05-2016"

# Hour and week offset in minutes
dayHourOffset <- 0

# Set the data path to the summary data
if(summaryType=="train"){
  dataPath <- paste0("../Downsampling/",targetDate,"/summary.rds")
} else{
  dataPath <- paste0("../Data/train.rds")
}

# Variation relaxation constants
varRelaxationC1 <- 300
varRelaxationC2 <- 20

# Daily density feature of given days
densityDays <- 0:550
nbDensityDays <- length(densityDays)

# Minimum place count in last cutoffDays[1] days of the time period for a place
# to be considered
# cutoffDays <- c(375,356,305,270,245,210,175,140,105,70,35,14)
cutoffDays <- 14*seq(27,1,-1)-7
cutoffWeeks <- seq(27,1,-1)
nbCutoffDays <- length(cutoffDays)
nbCutoffWeeks <- length(cutoffWeeks)
minPlaceCount <- 3

# Relative accuracy cutoff period in days
relAcCutoffDays <- 70

# Low week density count cutoff in number of checkins
lowWeekCheckinLimit <- 270000

# Gaussian smoothing of the 15-minute period (period must be odd!)
hourQuarterSmooth <- 15
hourQuarterDiffs <- (-(hourQuarterSmooth-1)/2):((hourQuarterSmooth-1)/2)
extremeSdHourQuarter <- 2
hourQuarterDens <- dnorm(hourQuarterDiffs/((hourQuarterSmooth-1)/2)*
                           extremeSdHourQuarter)
hourQuarterDens <- hourQuarterDens / sum(hourQuarterDens) # Gaussian smoother
# hourQuarterDens <- rep(1/hourQuarterSmooth, hourQuarterSmooth) # Uniform smth

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

# Gaussian smoothing of the combined hour-daily period (period must be odd!)
hourDaySmooth <- 9
hourDayDiffs <- (-(hourDaySmooth-1)/2):((hourDaySmooth-1)/2)
extremeSdHourDay <- 2.5
hourDayDens <- dnorm(hourDayDiffs/((hourDaySmooth-1)/2)*extremeSdHourDay)
hourDayDens <- hourDayDens / sum(hourDayDens) # Gaussian smoother
# hourDayDens <- rep(1/hourDaySmooth, hourDaySmooth) # Uniform smoother

# Gaussian smoothing of the running daily densities
dayDensitySmooth <- 15
dayDensDiffs <- (-(dayDensitySmooth-1)/2):((dayDensitySmooth-1)/2)
dayDensDiffs <- dayDensDiffs[-dayDensitySmooth]
extremeSdDayDensity <- 2
runningDayDens <- dnorm(dayDensDiffs/((dayDensitySmooth-1)/2)*
                          extremeSdDayDensity)
runningDayDens <- runningDayDens / sum(runningDayDens) # Gaussian smoother
# runningDayDens <- rep(1/runningDayDens, runningDayDens) # Uniform smoother

###################################################################

# Read in the summary data
summary <- readRDS(dataPath)


#####################################################
# Incorporate weekly time period density prediction #
#####################################################

# Load weekly place densities
fnWeeklyDensities <- paste(summaryType, "weekly densities.rds")
weeklyDensities <- readRDS(fnWeeklyDensities)

# Load weekly predictions
weeklyPredictionsFn <- paste(summaryType, "weekly predictions.rds")
weeklyPredictions <- readRDS(weeklyPredictionsFn)

# Set the predict horizon
if(summaryType == "train"){
  predictRange <- 58:77
} else{
  predictRange <- 77:100
}
nbPredicted <- length(predictRange)

# Create weekly predictions data table
weeklyPredictionsCombined <- data.table(place_id = weeklyDensities$place_id)

# Add weekly predictions for all considered time series methods
for(i in 1:nbPredicted){
  # Extract the considered week
  week <- predictRange[i]
  
  # Add arima forecasts
  colname <- paste0("weeklyArimaForecast",week)
  weeklyPredictionsCombined[, (colname) :=
                              weeklyPredictions$arimaPredictions[i,]]
  
  # Add exponential smoothing forecasts
  colname <- paste0("weeklySesForecast",week)
  weeklyPredictionsCombined[, (colname) :=
                              weeklyPredictions$sesPredictions[i,]]
  
  # Add holt forecasts
  colname <- paste0("weeklyHoltForecast",week)
  weeklyPredictionsCombined[, (colname) :=
                              weeklyPredictions$holtPredictions[i,]]
}

# Set the key of the weekly predictions
setkey(weeklyPredictionsCombined, place_id)

####################################
# Time period density calculations #
####################################

# Helper function to calculate the correlation between cyclic trends
cyclicCor <- function(x, period){
  itemsConsidered <- length(x) - period
  firstPart <- x[1:itemsConsidered]
  lastPart <- x[-(1:period)]
  relaxConstant <- (min(c(sum(firstPart!=0), sum(lastPart!=0)))-2)/
    (itemsConsidered-2) 
  if(relaxConstant > 0){
    itemCor <- cor(firstPart, lastPart)
  } else{
    itemCor <- NA
  }
  
  # Relax the correlation by the minimum of the number of non zero counts
  # of both ends
  out <- itemCor*relaxConstant
  out
}

# Calculate the correlation between subsequent 52 and 53 week periods
densitiesM <- as.matrix(weeklyDensities[, -1, with=FALSE])
week52Correlation <- apply(densitiesM, 1, cyclicCor, 52)
week53Correlation <- apply(densitiesM, 1, cyclicCor, 53)

# Compose the weekly correlations data table
weekCorrelations <- data.table(place_id = weeklyDensities$place_id,
                               week52Correlation = week52Correlation,
                               week53Correlation = week53Correlation)
setkey(weekCorrelations, place_id)

# Calculate time block cutoffs
timeCutoffs <- round(max(summary$time)-(1440*cutoffDays))
timeCutoffsWeek <- round(max(summary$time)-(1440*7*cutoffWeeks))

# Convert time to hour quarter of day 
summary[,hourQuarter := floor(((summary$time + dayHourOffset) %% 1440)/15)]

# Convert time to hour of day 
summary[,hour := floor(((summary$time + dayHourOffset) %% 1440)/60)]

# Convert time to day of week - maybe look for an optimal shift?
summary[,day :=
          floor(((summary$time + dayHourOffset) %% (1440*7)) / 1440)]

# Calculate the total day count
summary[,totalDay := floor((time + dayHourOffset)/(1440))]

# Convert time to hour day of week (0:(24*7-1))
summary[,hourDay := floor(((summary$time + dayHourOffset) %% (1440*7)) / 60)]

# Convert time to week and calculate the weekly fraction of checkins
summary[,week := floor((summary$time)/(1440*7))]
weekTrends <- summary[,list(N = length(x)*1440*7/(max(time)-min(time)),
                            medAc = as.numeric(median(accuracy)),
                            meanAc = mean(accuracy),
                            madAc = mad(accuracy),
                            meanLogAc = mean(log(1+accuracy))),
                      by=week]
weekTrends <- weekTrends[order(week),]

# Add the week counts to the summary data table
summary[, weekCount := weekTrends[1 + summary$week, N]]

# Normalize accuracy measure for time trend
relativeLogAccuracy <- log(1+summary$accuracy)/
  log(1+weekTrends[summary$week+1, medAc])
summary[,relativeLogAccuracy := relativeLogAccuracy]


# Calculate the fraction count during  the nbTimeCutoffs time blocks
# Fraction data table
densitySumm <- data.table(place_id=unique(summary$place_id))

# Set the place id as the key of placeSummary
setkey(densitySumm, place_id)

# Calculate relative densities over the time cutoff ranges
for(i in 0:nbCutoffDays){
  if(i==0){
    ids <- summary$time<=timeCutoffs[1]
  } else{
    if(i==nbCutoffDays){
      ids <- summary$time>timeCutoffs[i]
    } else{
      ids <- summary$time>timeCutoffs[i] & summary$time <= timeCutoffs[i+1]
    }
  }
  ids <- which(ids)
  nbIds <- length(ids)
  periodDensity <- summary[ids, .N/nbIds, by=place_id]
  
  matchIds <- match(periodDensity[, place_id], densitySumm[, place_id])
  colname <- paste0("periodDensity", i)
  
  # *1e6 for interpretability of the densities
  densitySumm[matchIds, (colname) := periodDensity[[2]]*1e6]
  densitySumm[-matchIds, (colname) := 0]
}

# Calculate relative densities over the final week cutoff ranges
for(i in 1:(nbCutoffWeeks-1)){
  ids <- summary$time>timeCutoffsWeek[i] &
    summary$time <= timeCutoffsWeek[i+1]
  ids <- which(ids)
  nbIds <- length(ids)
  periodDensity <- summary[ids, .N/nbIds, by=place_id]
  
  matchIds <- match(periodDensity[, place_id], densitySumm[,place_id])
  colname <- paste0("periodWeekDensity", 1 + nbCutoffWeeks - i)
  
  # *1e6 for interpretability of the densities
  densitySumm[matchIds,(colname) := periodDensity[[2]]*1e6]
  densitySumm[-matchIds,(colname) := 0]
}


# Calculate the daily density features
densityDays <- densityDays[densityDays <= max(summary$totalDay)]
nbDensityDays <- length(densityDays)
for(i in 1:nbDensityDays){
  densityDay <- densityDays[i]
  
  # Progress message
  cat("Processing density day", densityDay, "\n")
  
  ids <- summary$totalDay == densityDay
  ids <- which(ids)
  nbIds <- length(ids)
  dailyDensity <- summary[ids, .N/nbIds, by=place_id]
  
  matchIds <- match(dailyDensity[,place_id], densitySumm[,place_id])
  colname <- paste0("dailyDensity",densityDay)
  # *1e5 for interpretability of the densities
  densitySumm[matchIds,(colname) := dailyDensity[[2]]*1e5]
  densitySumm[-matchIds,(colname) := 0]
}

# Add the smoothed daily densities
for(i in 8:(nbDensityDays-6)){
  densityDay <- densityDays[i]
  
  # Progress message
  cat("Processing density day", densityDay, "\n")
  
  smoothDensSumG <- 0
  smoothDensSumU <- 0
  for(j in 1:(dayDensitySmooth-1)){
    dayDensDiff <- dayDensDiffs[j]
    day <- densityDay + dayDensDiff
    colname <- paste0("dailyDensity", day)
    smoothDensSumG <- smoothDensSumG + runningDayDens[j]*
      densitySumm[,colname, with=FALSE][[1]]
    smoothDensSumU <- smoothDensSumU + 1/dayDensitySmooth*
      densitySumm[,colname, with=FALSE][[1]]
  }
  colnameG <- paste0("dailyDensitySG", densityDay)
  densitySumm[,(colnameG) := smoothDensSumG]
  colnameU <- paste0("dailyDensitySU", densityDay)
  densitySumm[,(colnameU) := smoothDensSumU]
}

# Calculate the number of non-zero count days
dailyFreqs <- summary[,.N, by=list(place_id, totalDay)]
dailyCountFreqs <- dailyFreqs[,list(nonZeroDailyCount = .N), by=place_id]
dailyCountFreqs$nonZeroDailyRelativeCount <-
  dailyCountFreqs$nonZeroDailyCount/(max(dailyFreqs$totalDay))

# Set the place id as the key of dailyCountFreqs
setkey(dailyCountFreqs, place_id)


###############################
# Accuracy group calculations #
###############################

# Load accuracy summary features
accuracySummaryPath3 <- file.path(getwd(), targetDate,
                                  paste(summaryType,
                                        "accuracy summary features 3.rds"))
accuracyGroupSummary3 <- readRDS(accuracySummaryPath3)
accuracySummaryPath32 <- file.path(getwd(), targetDate,
                                   paste(summaryType,
                                         "accuracy summary features 32.rds"))
accuracyGroupSummary32 <- readRDS(accuracySummaryPath32)

# Add the accuracy groups
summary[,accuracyGroup3 :=
          cut(accuracy, breaks=c(accuracyGroupSummary3$lowCut, 1e5))]
summary[,accuracyGroup32 :=
          cut(accuracy, breaks=c(accuracyGroupSummary32$lowCut ,1e5))]

# Calculate the relative recent accuracy cutoff time
recentAcCutoffTime <- max(summary$time) - (relAcCutoffDays * 1440)
recentFractionPlaces <- summary[, mean(time > recentAcCutoffTime),
                                by=place_id][[2]]

# Calculate the busy fraction for all places
busyFractionPlaces <- summary[, mean(weekCount > lowWeekCheckinLimit),
                              by=place_id][[2]]

# Add the accuracy group characteristics
acGroupSummary <- summary[,.(groupCount = .N,
                             medX = median(x), madX = mad(x), sdX = sd(x),
                             medY = median(y), madY = mad(y), sdY = sd(y)),
                          by=place_id]
groupCount <- acGroupSummary$groupCount
acGroupSummary[,groupCount := NULL]
for(i in 1:3){
  # Add the count of the accuracy group
  colname <- paste0("countAcGroup3-", i)
  val <- summary[, sum(as.numeric(accuracyGroup3) == i), by=place_id][[2]]
  acGroupSummary[, (colname) := val]
  
  # Add the relative accuracy for the group
  colname <- paste0("relAcGroup3-", i)
  val <- summary[, mean(as.numeric(accuracyGroup3) == i), by=place_id][[2]]/
    accuracyGroupSummary3[i,relativeSize]
  acGroupSummary[, (colname) := val]
  
  # Add the recent relative accuracy for the group
  colname <- paste0("relRecAcGroup3-", i)
  val <- summary[, mean(as.numeric(accuracyGroup3) == i &
                          time > recentAcCutoffTime), by=place_id][[2]]/
    (accuracyGroupSummary3[i,relativeSize] * recentFractionPlaces)
  val[is.na(val)] <- acGroupSummary[[paste0("relAcGroup3-", i)]][is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Commented since very high correlation with the relative group accuracy
  # # Add the low density period excluded relative accuracy for the group
  # colname <- paste0("relBusyAcGroup3-", i)
  # val <- summary[, mean(as.numeric(accuracyGroup3) == i &
  #                         weekCount > lowWeekCheckinLimit), by=place_id][[2]]/
  #   (accuracyGroupSummary3[i,relativeSize] * busyFractionPlaces)
  # val[is.na(val)] <- acGroupSummary[[paste0("relAcGroup3-", i)]][is.na(val)]
  # acGroupSummary[, (colname) := val]
  
  # Add median x and mad x for the group
  # Median of x
  colname <- paste0("medXAcGroup", i)
  val <- summary[,median(as.numeric(x[as.numeric(accuracyGroup3) == i])),
                 by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$medX[is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Mad of x
  colname <- paste0("madXAcGroup", i)
  val <- summary[,mad(x[as.numeric(accuracyGroup3) == i]), by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$madX[is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Standard deviation of x
  colname <- paste0("sdXAcGroup", i)
  val <- summary[,sd(x[as.numeric(accuracyGroup3) == i]), by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$sdX[is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Add median y and mad y for the group
  # Median of y
  colname <- paste0("medYAcGroup", i)
  val <- summary[,median(as.numeric(y[as.numeric(accuracyGroup3) == i])),
                 by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$medY[is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Mad of y
  colname <- paste0("madYAcGroup", i)
  val <- summary[,mad(y[as.numeric(accuracyGroup3) == i]), by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$madY[is.na(val)]
  acGroupSummary[, (colname) := val]
  
  # Standard deviation of y
  colname <- paste0("sdYAcGroup", i)
  val <- summary[,sd(y[as.numeric(accuracyGroup3) == i]), by=place_id][[2]]
  val[is.na(val)] <- acGroupSummary$sdY[is.na(val)]
  acGroupSummary[, (colname) := val]
}

for(i in 1:32){
  # Add the count of the accuracy group
  colname <- paste0("countAcGroup32-", i)
  val <- summary[, sum(as.numeric(accuracyGroup32) == i), by=place_id][[2]]
  acGroupSummary[, (colname) := val]
  
  # Add the relative accuracy for the group
  colname <- paste0("relAcGroup32-", i)
  val <- summary[,mean(as.numeric(accuracyGroup32) == i), by=place_id][[2]]/
    accuracyGroupSummary32[i,relativeSize]
  acGroupSummary[, (colname) := val]
  
  # Add the recent relative accuracy for the group
  colname <- paste0("relRecAcGroup32-", i)
  val <- summary[, mean(as.numeric(accuracyGroup32) == i &
                          time > recentAcCutoffTime), by=place_id][[2]]/
    (accuracyGroupSummary32[i,relativeSize] * recentFractionPlaces)
  val[is.na(val)] <- acGroupSummary[[paste0("relAcGroup32-", i)]][is.na(val)]
  acGroupSummary[, (colname) := val]
}

# Drop the overall median, mads and sds
acGroupSummary[, c("medX", "madX", "sdX", "medY", "madY", "sdY") := NULL]

# Set the place id as the key of acGroupSummary
setkey(acGroupSummary, place_id)


# Summary table of the summary data by place_id
placeSummary <- summary[,list(count = length(accuracy),
                              recentCount = sum(time > recentAcCutoffTime),
                              relativeCount = length(accuracy)/nrow(summary)*
                                1e5,
                              # countLast175D = sum(time>=timeCutoffs[1]),
                              # countLast140D = sum(time>=timeCutoffs[2]),
                              # countLast105D = sum(time>=timeCutoffs[3]),
                              # countLast70D = sum(time>=timeCutoffs[4]),
                              # countLast35D = sum(time>=timeCutoffs[5]),
                              # countLast14D = sum(time>=timeCutoffs[6]),
                              minTime = min(time),
                              maxTime = max(time),
                              
                              medX=as.double(median(x)), madX=mad(x), sdX=sd(x), 
                              medY=as.double(median(y)), madY=mad(y), sdY=sd(y),
                              
                              medAc=as.double(median(accuracy)),
                              madAc=mad(accuracy),
                              
                              medRelAc = median(exp(relativeLogAccuracy)-1),
                              madRelAc = mad(exp(relativeLogAccuracy)-1),
                              medRelLogAc = median(relativeLogAccuracy),
                              madRelLogAc = mad(relativeLogAccuracy)
),
by=place_id]

# Add the relative recent count
placeSummary[, relativeRecentCount := recentCount/
               sum(summary$time > recentAcCutoffTime) * 1e5]

# Set the place id as the key of placeSummary
setkey(placeSummary, place_id)

# Merge place summary with the density summary
placeSummary <- placeSummary[densitySumm]

# Merge with daily count frequencies
placeSummary <- placeSummary[dailyCountFreqs]

# Merge with weekly correlations
placeSummary <- placeSummary[weekCorrelations]

# Merge with the combined weekly predictions
placeSummary <- placeSummary[weeklyPredictionsCombined]

# Drop places that have less than minPlaceCount observations in the last 50
# percent of the studied time period
# Modificiation: DO NOT do this anymore - maybe a yearly seasonality!
# countLast10Perc >= minPlaceCount |
# placeSummary <- placeSummary[countLast175D >= minPlaceCount,]

# Drop count column and use relative frequencies instead
# placeSummary[, countLast175D := NULL]

# Merge with accuracy group summary
placeSummary <- placeSummary[acGroupSummary]

# Drop places that have less than three observations
placeSummary <- placeSummary[count >= 3,]


###################################
# Section 1: Location features    #
# Relax the MADs of the locations #
###################################

# Obtain the the mean mads
madWeights <- (placeSummary[,count]-1)
meanMadX <- mean(placeSummary[,madX]*madWeights/mean(madWeights), na.rm = TRUE)
meanMadY <- mean(placeSummary[,madY]*madWeights/mean(madWeights), na.rm = TRUE)
meanSdX <- mean(placeSummary[,sdX]*madWeights/mean(madWeights), na.rm = TRUE)
meanSdY <- mean(placeSummary[,sdY]*madWeights/mean(madWeights), na.rm = TRUE)

# Relaxation in the x direction
# Median absolute deviation from the median
relaxedMadX <- (placeSummary[,madX]*placeSummary[,count]+
                  meanMadX*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedMadX <- (placeSummary[,madX]*placeSummary[,count]+
                          meanMadX*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedMadX[is.na(relaxedMadX)] <- meanMadX
slightlyRelaxedMadX[is.na(slightlyRelaxedMadX)] <- meanMadX
placeSummary$relaxedMadX <- relaxedMadX
placeSummary$slightlyRelaxedMadX <- slightlyRelaxedMadX

# Standard deviation
relaxedSdX <- (placeSummary[,sdX]*placeSummary[,count]+
                  meanSdX*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedSdX <- (placeSummary[,sdX]*placeSummary[,count]+
                          meanSdX*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedSdX[is.na(relaxedSdX)] <- meanSdX
slightlyRelaxedSdX[is.na(slightlyRelaxedSdX)] <- meanSdX
placeSummary$relaxedSdX <- relaxedSdX
placeSummary$slightlyRelaxedSdX <- slightlyRelaxedSdX

# Relaxation in the y direction
# Median absolute deviation from the median
relaxedMadY <- (placeSummary[,madY]*placeSummary[,count]+
                  meanMadY*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedMadY <- (placeSummary[,madY]*placeSummary[,count]+
                          meanMadY*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedMadY[is.na(relaxedMadY)] <- meanMadY
slightlyRelaxedMadY[is.na(slightlyRelaxedMadY)] <- meanMadY
placeSummary$relaxedMadY <- relaxedMadY
placeSummary$slightlyRelaxedMadY <- slightlyRelaxedMadY

# Standard deviation
relaxedSdY <- (placeSummary[,sdY]*placeSummary[,count]+
                 meanSdY*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedSdY <- (placeSummary[,sdY]*placeSummary[,count]+
                         meanSdY*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedSdY[is.na(relaxedSdY)] <- meanSdY
slightlyRelaxedSdY[is.na(slightlyRelaxedSdY)] <- meanSdY
placeSummary$relaxedSdY <- relaxedSdY
placeSummary$slightlyRelaxedSdY <- slightlyRelaxedSdY

# Add ratios madX/madY and relaxedMadX/relaxedMadY
placeSummary[,ratioMadXY:=madX/madY]
placeSummary[,relaxedRatioMadXY:=relaxedMadX/relaxedMadY]


##############################
# Section 2: Time features   #
# 1) Hourly quarter features #  
# 2) Hourly features         #
# 3) Daily features          #
# 4) Hour daily features     #
##############################

# 1) Hourly quarter features
# Add hourly quarter fractions to the top places data table
for(i in 0:95){
  # Progress message
  cat("Processing mean hourly quarter", i, "\n")
  
  colname <- paste0("hourQuarterMean", i)
  hourQuarterMeans <- summary[, mean(hourQuarter==i), by=place_id]
  setkey(hourQuarterMeans, "place_id")
  
  # Drop unmatched hour quarter means
  hourQuarterMeans <- hourQuarterMeans[place_id %in% placeSummary$place_id]
  
  # Rename to colname
  setnames(hourQuarterMeans, "V1", colname)
  
  # Join with top place data
  placeSummary <- placeSummary[hourQuarterMeans]
}

# Add smoothed hourly quarter fractions to the top places data table
for(i in 0:95){
  # Progress message
  cat("Processing smoothed hour quarter", i, "\n")
  
  smoothHourQuarterSum <- 0
  for(j in 1:hourQuarterSmooth){
    hourQuarterDiff <- hourQuarterDiffs[j]
    hourQuarter <- (i+hourQuarterDiff) %% 96
    colname <- paste0("hourQuarterMean",  hourQuarter)
    smoothHourQuarterSum <- smoothHourQuarterSum + hourQuarterDens[j]*
      placeSummary[, colname, with=FALSE][[1]]
  }
  colname <- paste0("hourQuarterMeanS", i)
  placeSummary[, (colname) := smoothHourQuarterSum]
}

# 2) Hourly features
# Add hourly fractions to the top places data table
for(i in 0:23){
  # Progress message
  cat("Processing mean hour",i,"\n")
  
  colname <- paste0("hourMean",i)
  hourMeans <- summary[, mean(hour==i), by=place_id]
  setkey(hourMeans, "place_id")
  
  # Drop unmatched hour means
  hourMeans <- hourMeans[place_id %in% placeSummary$place_id]
  
  # Rename to colname
  setnames(hourMeans,"V1",colname)
  
  # Join with top place data
  placeSummary <- placeSummary[hourMeans]
}

# Add smoothed hourly fractions to the top places data table
for(i in 0:23){
  # Progress message
  cat("Processing smoothed hour", i, "\n")
  
  smoothHourSum <- 0
  for(j in 1:hourSmooth){
    hourDiff <- hourDiffs[j]
    hour <- (i+hourDiff) %% 24
    colname <- paste0("hourMean",hour)
    smoothHourSum <- smoothHourSum + hourDens[j]*
      placeSummary[,colname, with=FALSE][[1]]
  }
  colname <- paste0("hourMeanS",i)
  placeSummary[,(colname) := smoothHourSum]
}


# 3) Daily features
# Add hourly fractions to the top places data table
for(i in 0:6){
  # Progress message
  cat("Processing mean day",i,"\n")
  
  colname <- paste0("dayMean",i)
  dayMeans <- summary[, mean(day==i), by=place_id]
  setkey(dayMeans, "place_id")
  
  # Drop unmatched day means
  dayMeans <- dayMeans[place_id %in% placeSummary$place_id]
  
  # Rename to colname
  setnames(dayMeans,"V1",colname)
  
  # Merge with top place data
  placeSummary <- placeSummary[dayMeans]
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
      placeSummary[,colname, with=FALSE][[1]]
  }
  colname <- paste0("dayMeanS",i)
  placeSummary[,(colname) := smoothDaySum]
}

# 4) Hour-daily features
# Add hour-daily fractions to the top places data table
for(i in 0:167){
  # Progress message
  cat("Processing mean hour-day",i,"\n")
  
  colname <- paste0("hourDayMean",i)
  hourDayMeans <- summary[, mean(hourDay==i), by=place_id]
  setkey(hourDayMeans, "place_id")
  
  # Drop unmatched hour means
  hourDayMeans <- hourDayMeans[place_id %in% placeSummary$place_id]
  
  # Rename to colname
  setnames(hourDayMeans,"V1",colname)
  
  # Join with top place data
  placeSummary <- placeSummary[hourDayMeans]
}

# Add smoothed hour-daily fractions to the top places data table
for(i in 0:167){
  # Progress message
  cat("Processing smoothed hour-day", i, "\n")
  
  smoothHourDaySum <- 0
  for(j in 1:hourDaySmooth){
    hourDayDiff <- hourDayDiffs[j]
    hourDay <- (i+hourDayDiff) %% 168
    colname <- paste0("hourDayMean",hourDay)
    smoothHourDaySum <- smoothHourDaySum + hourDayDens[j]*
      placeSummary[,colname, with=FALSE][[1]]
  }
  colname <- paste0("hourDayMeanS",i)
  placeSummary[,(colname) := smoothHourDaySum]
}

#################################
# Section 3: Accuracy features  #
#################################

# Calculate the mean Mad of the accuracy
meanMadAc <- mean(placeSummary[,madAc]*madWeights/mean(madWeights),
                  na.rm = TRUE)
meanMedRelLogAc <- mean(placeSummary[,medRelLogAc]*madWeights/mean(madWeights),
                        na.rm = TRUE)
meanMadRelLogAc <- mean(placeSummary[,madRelLogAc]*madWeights/
                          mean(madWeights), na.rm = TRUE)

# Relaxation in the Ac direction
# Median absolute deviation from the median
relaxedMadAc <- (placeSummary[,madAc]*placeSummary[,count]+
                   meanMadAc*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedMadAc <- (placeSummary[,madAc]*placeSummary[,count]+
                           meanMadAc*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedMadAc[is.na(relaxedMadAc)] <- meanMadAc
slightlyRelaxedMadAc[is.na(slightlyRelaxedMadAc)] <- meanMadAc
placeSummary$relaxedMadAc <- relaxedMadAc
placeSummary$slightlyRelaxedMadAc <- slightlyRelaxedMadAc

# Median relative log accuracy
relaxedMedRelLogAc <- (placeSummary[,medRelLogAc]*placeSummary[,count]+
                         meanMedRelLogAc*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedMedRelLogAc <- (placeSummary[,medRelLogAc]*placeSummary[,count]+
                                 meanMedRelLogAc*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedMedRelLogAc[is.na(relaxedMedRelLogAc)] <- meanMedRelLogAc
slightlyRelaxedMedRelLogAc[is.na(slightlyRelaxedMedRelLogAc)] <-
  meanMedRelLogAc
placeSummary$relaxedMedRelLogAc <- relaxedMedRelLogAc
placeSummary$slightlyRelaxedMedRelLogAc <- slightlyRelaxedMedRelLogAc

# Mad relative log accuracy
relaxedMadRelLogAc <- (placeSummary[,madRelLogAc]*placeSummary[,count]+
                         meanMadRelLogAc*varRelaxationC1)/
  (placeSummary[,count]+varRelaxationC1)
slightlyRelaxedMadRelLogAc <- (placeSummary[,madRelLogAc]*placeSummary[,count]+
                                 meanMadRelLogAc*varRelaxationC2)/
  (placeSummary[,count]+varRelaxationC2)
relaxedMadRelLogAc[is.na(relaxedMadRelLogAc)] <- meanMadRelLogAc
slightlyRelaxedMadRelLogAc[is.na(slightlyRelaxedMadRelLogAc)] <-
  meanMadRelLogAc
placeSummary$relaxedMadRelLogAc <- relaxedMadRelLogAc
placeSummary$slightlyRelaxedMadRelLogAc <- slightlyRelaxedMadRelLogAc

###############################
# Section 4: Generate output  #
###############################

# Write the place summary to the target date folder
folderPath <- file.path(getwd(), targetDate)
dir.create(folderPath, showWarnings = FALSE)
fn <- paste0(summaryType, " summary features.rds")
saveRDS(placeSummary, file.path(folderPath, fn))