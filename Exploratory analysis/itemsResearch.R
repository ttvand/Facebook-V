# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Exploratory analysis")

# Load the required libraries
library(data.table)
library(bit64)
library(ggplot2)
library(plotly)
library(viridis)
library(ggvis)

# Target date
targetDate <- "23-05-2016"

# Do not set seed in order to avoid reproducible results
# set.seed(14)

# Maybe even zoom in further than hour?

# Hour and week offset in minutes
dayHourOffset <- 0

# Number of studied top places and ranking of first studied place
nbStudiedPlaces <- 10

# Train - Validation cutoff time
trainTimeLimit <- 587000

# Option to skip the overall analysis
skipOverallAnalysis <- TRUE


################################################################"

# Read in the training data and order by time
train <- readRDS("../Data/train.rds")
train <- train[order(time)]

# Load summary table of the train data by place_id
summaryFilePath <- paste0("../Feature engineering/", targetDate,
                          "/test summary features.rds")
placeSummary <- readRDS(summaryFilePath)

max(diff(train$time)) # 1 minute => No daylight saving issues!
mean(diff(train$time)) # About 40 checkins a minute

# Add minute, hour, day week and time part (train/validation)
train[,minute := (time + dayHourOffset) %% 60]
train[,hour := floor(((time + dayHourOffset) %% 1440)/60)]
train[,totalHour := floor((time + dayHourOffset)/(60))]
train[,day := floor(((time + dayHourOffset) %% (1440*7)) / 1440)]
train[,totalDay := floor((time + dayHourOffset)/(1440))]
train[,week := floor(time/(1440*7))]
train[,timeHalf := ifelse(time<trainTimeLimit,"Train","Validation")]
train[,oddWeek := ifelse(week %% 2 == 0,"Even","Odd")]

if(!skipOverallAnalysis){
  # Sample from train in order to study the univariate time  distribution
  trainSample <- train[sample(1:nrow(train),round(nrow(train)/10))]
  
  # Analysis I: Check-in count versus time
  # Plot density of time parts
  ggplot(trainSample, aes(x=week)) + xlim(0,max(trainSample$week)) + geom_bar()
  # Two major dips!
  ggplot(trainSample, aes(x=day)) + xlim(0,max(trainSample$day)) + geom_bar()
  ggplot(trainSample, aes(x=hour)) + xlim(0,max(trainSample$hour)) + geom_bar()
  
  # Analysis II: Check-in count versus location
  trainSample$count <- placeSummary[match(trainSample$place_id,place_id), count]
  trainSample <- trainSample[!is.na(count)]
  ggplot(trainSample, aes(x, y, z = count)) +
    stat_summary_2d(fun = length, bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample, aes(x, y, z = count)) +
    stat_summary_2d(fun = mean, bins = 50) +
    scale_fill_viridis()
  
  # Maybe some area went out of signal? Nope
  weekCount <- trainSample[,length(x),by=week]
  weekCount <- weekCount[[2]][order(weekCount[[1]])]
  ggplot(trainSample[weekCount[week+1]<17500,], aes(x, y, z = count)) +
    stat_summary_2d(fun = mean, bins = 50) +
    scale_fill_viridis()
  
  # Analysis III: Check-in count versus accuracy
  p <- ggplot(trainSample, aes(accuracy)) +
    geom_density()
  ggplotly(p)
  
  # Analysis IV: Week versus location
  # The weekly density comes back but the lower density at x=1 and at the
  # edges is remarkable. Nothing major though
  # The y-density is lower at the edges as well => use as an additional pred? 
  ggplot(trainSample, aes(x, week, z = count)) +
    stat_summary_2d(fun = length, bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample, aes(y, week, z = count)) +
    stat_summary_2d(fun = length, bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample, aes(x, y, z = week)) +
    stat_summary_2d(fun = length, bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample, aes(x, y, z = week)) +
    stat_summary_2d(fun = length, bins = 50) +
    scale_fill_viridis()
  
  ggplot(trainSample[week<70], aes(x = blockId, y = week)) + 
    geom_bin2d(bins = 30)
  p <- ggplot(trainSample, aes(x)) + geom_density(alpha=.3)
  p <- ggplot(trainSample, aes(x)) + geom_histogram(bins=100)
  ggplotly(p)
  p <- ggplot(trainSample, aes(y)) + geom_density()
  p <- ggplot(trainSample, aes(y)) + geom_histogram(bins=100)
  ggplotly(p)
  
  # Redo the plot with time instead of week - No pattern, weird.
  ggplot(trainSample, aes(x = x, y = totalHour)) + 
    geom_bin2d(bins = 250)
  
  # Zoom in between weeks 60 and 70 - Also no pattern
  trainSampleTimeZoom <- trainSample[week>60 & week<=70,]
  ggplot(trainSampleTimeZoom, aes(x, totalDay, z = count)) +
    stat_summary_2d(fun = length, bins = 49) +
    scale_fill_viridis()
  table(trainSampleTimeZoom$totalDay)
  
  # Fourrier analysis might reveal what's going on - all good!
  
  
  # Analysis V: Day of the week versus location
  ggplot(trainSample[day==0], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==1], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==2], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==3], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==4], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==5], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[day==6], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  
  # Analysis VI: Hour of the day versus location
  ggplot(trainSample[hour <= 4], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[hour > 4 & hour <= 8], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[hour > 8 & hour <= 12], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[hour > 12 & hour <= 16], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[hour > 16 & hour <= 20], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  ggplot(trainSample[hour > 20], aes(x, y)) +
    geom_bin2d(bins = 50) +
    scale_fill_viridis()
  
  # Analysis VII: Week versus accuracy - sliding density
  ggplot(trainSample[week > 0 & week <= 4], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 4 & week <= 8], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 8 & week <= 12], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 12 & week <= 16], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 16 & week <= 20], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 20 & week <= 24], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 24 & week <= 28], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 28 & week <= 32], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 32 & week <= 36], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 36 & week <= 40], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 40 & week <= 44], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 44 & week <= 48], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 48 & week <= 52], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 52 & week <= 56], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 56 & week <= 60], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 60 & week <= 64], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 64 & week <= 68], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 68 & week <= 72], aes(accuracy)) + geom_density()
  ggplot(trainSample[week > 72 & week <= 76], aes(accuracy)) + geom_density()
  
  # Analysis VIII: Day of the week versus accuracy
  ggplot(trainSample[accuracy<250], aes(x=day, y=accuracy, group=day)) + 
    geom_boxplot()
  
  # Analysis IX: Hour of the day versus accuracy
  ggplot(trainSample[accuracy<250], aes(x=hour, y=accuracy, group=hour)) + 
    geom_boxplot()
  
  # Analysis X: Location versus accuracy
  ggplot(trainSample, aes(x, y, z = accuracy)) +
    stat_summary_2d(fun = median, bins = 50) +
    scale_fill_viridis()
}


#######################################################################
# Study specific place ids to get a better idea of the true variation #
# Random sampling so that larger places have higher occurence probs   #
#######################################################################

# Extract studied places
studiedPlaces <- placeSummary[order(-count),place_id]
studyTrainIds <- sample(1:nrow(train), nbStudiedPlaces*10)
studiedPlaces <- unique(train[studyTrainIds, place_id])[1:nbStudiedPlaces]
studiedPlaceData <- train[place_id %in% studiedPlaces,]

# Analysis I: Check-in count versus time (univariate)
# Year cyclic trend?
countsTimeBack <- function(studiedPlaceData, timeWindow, daysInPeriod){
  
  topPlaceDataSecondY <- studiedPlaceData[totalDay>daysInPeriod+timeWindow]
  nbTopSecondY <- nrow(topPlaceDataSecondY)
  # Count the number of checkins that also had a checkin in the same time period
  # of a year back
  prevYearCheck <- rep(NA,nbTopSecondY)
  for(i in 1:nbTopSecondY){
    checkDay <- topPlaceDataSecondY[i,totalDay]
    prevYearCheck[i] <- topPlaceDataSecondY[i,place_id] %in%
      studiedPlaceData[totalDay >= checkDay - daysInPeriod - timeWindow &
                     totalDay <= checkDay - daysInPeriod + timeWindow,
                   place_id]
  }
  mean(prevYearCheck)
}

# # No obvious yearly trend
# countsTimeBack(studiedPlaceData, 7, 365)
# countsTimeBack(studiedPlaceData, 7, 345)
# countsTimeBack(studiedPlaceData, 7, 325)
# countsTimeBack(studiedPlaceData, 7, 305)

# Other cyclic trend? Kinda!
windowSize <- 1
timesBack <- 10*(1:40)
# timePeriodFreqs <-
#   sapply(timesBack, function(x) countsTimeBack(studiedPlaceData,windowSize,x))
# plot(timesBack, timePeriodFreqs, xlab = "Days back")

# # Weekly trend of top places
# for(i in 1:(nbStudiedPlaces)){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]], aes(x=week)) +
#     xlim(0,80) +
#     geom_bar()
#   p <- p +
#     ggtitle(paste("Studied place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# blockMinSize <- 30
# studiedPlaceData[,minuteBlock := floor(((time + dayHourOffset) %% 1440)/
#                                          blockMinSize)]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x=minuteBlock)) +
#     xlim(0, max(studiedPlaceData$minuteBlock)) +
#     geom_bar() +
#     facet_wrap(~timeHalf,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis II: Check-in count versus location
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x,y)) +
#     geom_point() +
#     # facet_wrap(~timeHalf,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis III: Check-in count versus accuracy
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(accuracy)) +
#     geom_density() +
#     xlim(0,250) +
#     # facet_wrap(~timeHalf,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis IV: Week versus location
# studiedPlaceData[,weekCat := factor(cut(week,4))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x,y,col=weekCat)) +
#     geom_point() +
#     facet_wrap(~weekCat,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis V: Day of the week versus location
# studiedPlaceData[,dayFac:=factor(day)]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x,y,col=dayFac)) +
#     geom_point() +
#     # facet_wrap(~day,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # EXTRA Analysis: Minute versus location - Nothing here
# studiedPlaceData[,minuteFac:=factor(cut(minute,6))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(minute)) +
#     geom_histogram() +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis VI: Hour of the day versus location
# studiedPlaceData[,hourFac:=factor(cut(hour,4))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x,y,col=hourFac)) +
#     geom_point() +
#     # facet_wrap(~hourFac,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# studiedPlaceData[,hourFac2:=factor(cut(hour,12))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x,y,col=hourFac2)) +
#     geom_point() +
#     facet_wrap(~hourFac2,nrow=2) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis VII: Week versus accuracy
# # Looking at the quantiles would be appropriate here (TODO)
# studiedPlaceData[,weekFac:=factor(cut(week,10))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]][accuracy<250],
#          aes(x=weekFac, y=accuracy, group=weekFac, fill=weekFac)) + 
#     geom_boxplot() +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis VIII: Day of the week versus accuracy
# # Looking at the quantiles would be appropriate here (TODO)
# studiedPlaceData[,dayFac:=factor(day)]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]][accuracy<250],
#               aes(x=dayFac, y=accuracy, group=dayFac, fill=dayFac)) + 
#     geom_boxplot() +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis IX: Hour of the day versus accuracy
# # Looking at the quantiles would be appropriate here (TODO)
# studiedPlaceData[,hourFac:=factor(cut(hour,4))]
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]][accuracy<250],
#               aes(x=hourFac, y=accuracy, group=hourFac, fill=hourFac)) + 
#     geom_boxplot() +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis X: Location versus accuracy
# for(i in 1:nbStudiedPlaces){
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x, y, z = accuracy)) +
# stat_summary_2d(fun = median, bins = 30) +
#   scale_fill_viridis() +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis XI: Hour of the day versus day of the week
# for(i in 1:nbStudiedPlaces){
#   nbBins <- 1+diff(range(studiedPlaceData[place_id==studiedPlaces[i],hour]))
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x=hour)) +
#     geom_histogram(bins = nbBins) +
#     facet_wrap(c("day")) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }
# 
# # Analysis XII: Hour of the day versus day of the week and time half
# for(i in 1:nbStudiedPlaces){
#   nbBins <- 1+diff(range(studiedPlaceData[place_id==studiedPlaces[i],hour]))
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x=hour)) +
#     geom_histogram(bins = nbBins) +
#     facet_wrap(c("day","timeHalf")) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }

# # Analysis XIII: Hour of the day versus day of the week and odd week
# for(i in 1:nbStudiedPlaces){
#   nbBins <- 1+diff(range(studiedPlaceData[place_id==studiedPlaces[i],hour]))
#   p <- ggplot(studiedPlaceData[place_id==studiedPlaces[i]],
#               aes(x=hour)) +
#     geom_histogram(bins = nbBins) +
#     facet_wrap(c("day","oddWeek")) +
#     ggtitle(paste("Top place",i))
#   print(p)
#   cat("Press [enter] to continue")
#   line <- readline()
# }
