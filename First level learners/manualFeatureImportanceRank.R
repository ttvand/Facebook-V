# Logic that orders and stores the considered features
# The order of the feature is important for the first level learners since
# not all techniques will consider all predictors and some will only consider
# the top L predictors
# - Combination of feature rank in xgboost selection
# - Finetuned by personal judgment - Limited changes required!

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/First level learners")

# Target save files
saveFile <- "orderedFeatures.rds"
saveFileNoMissing <- "orderedFeaturesNoMissing.rds"

# Nearest neighbor parameters
nbKs <- 10
nbDifferentKCounts <- 7
nbMidKs <- 40

# All predictors
allPredictors <- c(paste0("k", 1:(nbKs*nbDifferentKCounts))
                   , paste0("kInt", 1:nbKs)
                   , paste0("midK", 1:nbMidKs)
                   , paste0("neighborDistanceMidK", 1:nbMidKs)
                   , paste0("meanTimeDiff", 1:(nbKs*nbDifferentKCounts))
                   , paste0("neighborDistance", 1:nbKs)
                   , "timeSinceSummary" # DANGEROUS predictor wrt confounding!
                   , "logTimeSinceSummaryDays"
                   , "noCheckTimeSumEnd"
                   , "relativeAccuracyZ", "logRelativeAccuracyZ"
                   , "relativeRelaxedAccuracyZ", "logRelativeRelaxedAccuracyZ"
                   , "relativeSlightlyRelaxedAccuracyZ"
                   , "logRelativeSlightlyRelaxedAccuracyZ"
                   , "madRelAc", "madRelLogAc",
                   "relaxedMadRelLogAc", "slightlyRelaxedMadRelLogAc"
                   , "relativeCount", "nonZeroDailyRelativeCount",
                   "relativeRecentCount"
                   # , "count", "nonZeroDailyCount"
                   # , "countLast175D", "countLast70D", "countLast35D"
                   , "hourQuarterDens", "hourQuarterDensR",
                   "smoothHourQuarterDens", "smoothHourQuarterDensR"
                   , "hourDens", "hourDensR", "smoothHourDens", "smoothHourDensR"
                   , "dayDens", "dayDensR", "smoothDayDens", "smoothDayDensR"
                   , "hourDayDens", "hourDayDensR", "smoothHourDayDens",
                   "smoothHourDayDensR"
                   , "x", "y", "xy"
                   , "madX", "relaxedMadX", "slightlyRelaxedMadX"
                   , "madY", "relaxedMadY", "slightlyRelaxedMadY"
                   , "sdX", "relaxedSdX", "slightlyRelaxedSdX"
                   , "sdY", "relaxedSdY", "slightlyRelaxedSdY"
                   , "xZ", "xZLog", "xZLog2", "yZ", "yZLog", "yZLog2"
                   , "xZSlightRelax", "yZSlightRelax"
                   , "relaxedMadRatio", "logRelaxedMadRatio"
                   , "slightlyRelaxedMadRatio", "logSlightlyRelaxedMadRatio"
                   , paste0("periodDensity", 0:27)
                   , paste0("weekBackDensity", seq(75,1,-2))
                   , paste0("periodWeekDensity", 2:27)
                   , "weeklyCount"
                   , "distanceXBorder", "distanceYBorder"
                   , "distanceXLowBorder", "distanceXHighBorder"
                   , "distanceYLowBorder", "distanceYHighBorder"
                   , "timeOpen", "isAtLeastOpenYear"
                   , "dayDens52WeekBack", "smoothGDayDens52WeekBack"
                   , "smoothUDayDens52WeekBack"
                   , "dayDens53WeekBack", "smoothGDayDens53WeekBack"
                   , "week52Correlation", "week53Correlation"
                   # , "smoothUDayDens53WeekBack"
                   , "accuracy", "isAcGroup1", "isAcGroup2", "isAcGroup3"
                   , "madXValAcGroup", "sdXAcGroup",
                   "madYValAcGroup", "sdYAcGroup"
                   , "xZAcGroup", "yZAcGroup"
                   , "acGroup3Density", "acGroup3DensityR"
                   , "acGroup32Density", "acGroup32DensityR"
                   , "acGroup3RecDensity", "acGroup3RecDensityR"
                   , "acGroup32RecDensity", "acGroup32RecDensityR"
                   , "regionDensity", "smoothRegionDensity"
                   , "predictWeekDensityArima", "predictWeekDensitySes"
                   , "predictWeekDensityHolt"
)

# Top 285 predictors
top285Predictors <- c("smoothHourDens", "kInt5", "midK13", "k5", "smoothHourDayDens", 
  "smoothHourDensR", "midK19", "midK18", "midK17", "hourDayDens", 
  "k4", "midK24", "kInt6", "midK12", "acGroup3RecDensityR", "relativeAccuracyZ", 
  "hourDens", "midK23", "acGroup32RecDensityR", "smoothHourDayDensR", 
  "smoothHourQuarterDensR", "hourDensR", "xZAcGroup", "acGroup32Density", 
  "k24", "midK14", "acGroup3RecDensity", "smoothDayDens", "relativeCount", 
  "acGroup3Density", "smoothHourQuarterDens", "k15", "yZ", "smoothDayDensR", 
  "midK22", "hourQuarterDens", "accuracy", "yZAcGroup", "relativeRelaxedAccuracyZ", 
  "x", "noCheckTimeSumEnd", "hourQuarterDensR", "y", "dayDens", 
  "acGroup32RecDensity", "hourDayDensR", "xZ", "distanceXBorder", 
  "madXValAcGroup", "acGroup32DensityR", "relativeSlightlyRelaxedAccuracyZ", 
  "distanceYBorder", "regionDensity", "xy", "smoothRegionDensity", 
  "dayDens52WeekBack", "neighborDistance1", "sdXAcGroup", "dayDensR", 
  "sdYAcGroup", "xZSlightRelax", "week52Correlation", "predictWeekDensityHolt", 
  "sdX", "weekBackDensity17", "timeOpen", "madYValAcGroup", "madRelLogAc", 
  "week53Correlation", "smoothGDayDens52WeekBack", "sdY", "nonZeroDailyRelativeCount", 
  "weekBackDensity19", "neighborDistance10", "weekBackDensity15", 
  "acGroup3DensityR", "neighborDistance9", "kInt3", "madY", "timeSinceSummary", 
  "yZSlightRelax", "midK7", "madRelAc", "predictWeekDensitySes", 
  "weekBackDensity59", "weekBackDensity61", "predictWeekDensityArima", 
  "relaxedMadRelLogAc", "yZLog", "weekBackDensity35", "weekBackDensity63", 
  "weekBackDensity13", "madX", "neighborDistance8", "weekBackDensity25", 
  "slightlyRelaxedMadRatio", "weekBackDensity33", "neighborDistance2", 
  "neighborDistanceMidK1", "meanTimeDiff14", "weekBackDensity23", 
  "midK16", "weekBackDensity31", "weekBackDensity29", "weekBackDensity65", 
  "weekBackDensity11", "relaxedSdX", "weekBackDensity51", "logRelativeSlightlyRelaxedAccuracyZ", 
  "smoothUDayDens52WeekBack", "weekBackDensity37", "weekBackDensity39", 
  "weekBackDensity67", "k61", "slightlyRelaxedSdX", "kInt1", "weekBackDensity53", 
  "meanTimeDiff57", "meanTimeDiff16", "logRelativeAccuracyZ", "meanTimeDiff15", 
  "weekBackDensity45", "slightlyRelaxedMadRelLogAc", "meanTimeDiff18", 
  "meanTimeDiff17", "meanTimeDiff65", "midK27", "k14", "weekBackDensity27", 
  "weekBackDensity47", "meanTimeDiff68", "weekBackDensity41", "meanTimeDiff66", 
  "meanTimeDiff5", "kInt4", "weekBackDensity49", "midK11", "weekBackDensity21", 
  "dayDens53WeekBack", "periodDensity0", "meanTimeDiff3", "weekBackDensity9", 
  "meanTimeDiff64", "weekBackDensity69", "meanTimeDiff13", "relaxedMadRatio", 
  "kInt2", "weekBackDensity43", "logRelativeRelaxedAccuracyZ", 
  "meanTimeDiff67", "slightlyRelaxedSdY", "weekBackDensity57", 
  "meanTimeDiff19", "weekBackDensity7", "meanTimeDiff26", "meanTimeDiff54", 
  "midK6", "meanTimeDiff6", "meanTimeDiff69", "neighborDistanceMidK36", 
  "periodDensity27", "weekBackDensity55", "slightlyRelaxedMadX", 
  "periodWeekDensity2", "k51", "neighborDistanceMidK2", "meanTimeDiff7", 
  "meanTimeDiff24", "meanTimeDiff20", "meanTimeDiff36", "meanTimeDiff56", 
  "periodDensity21", "meanTimeDiff12", "meanTimeDiff55", "slightlyRelaxedMadY", 
  "meanTimeDiff9", "meanTimeDiff70", "relaxedSdY", "midK21", "meanTimeDiff4", 
  "neighborDistance7", "neighborDistanceMidK5", "smoothGDayDens53WeekBack", 
  "meanTimeDiff46", "distanceYLowBorder", "relaxedMadX", "meanTimeDiff28", 
  "yZLog2", "meanTimeDiff35", "meanTimeDiff25", "meanTimeDiff45", 
  "weekBackDensity71", "kInt7", "neighborDistanceMidK15", "meanTimeDiff63", 
  "meanTimeDiff44", "meanTimeDiff58", "meanTimeDiff49", "distanceXLowBorder", 
  "k3", "periodWeekDensity3", "k12", "meanTimeDiff47", "meanTimeDiff62", 
  "relativeRecentCount", "periodWeekDensity15", "k13", "xZLog", 
  "k62", "meanTimeDiff59", "periodWeekDensity4", "meanTimeDiff48", 
  "periodDensity8", "k52", "meanTimeDiff27", "neighborDistanceMidK40", 
  "midK4", "periodDensity22", "neighborDistanceMidK4", "meanTimeDiff38", 
  "periodDensity25", "neighborDistanceMidK10", "k25", "periodDensity13", 
  "neighborDistanceMidK6", "meanTimeDiff10", "weeklyCount", "periodDensity20", 
  "periodDensity26", "meanTimeDiff8", "logTimeSinceSummaryDays", 
  "meanTimeDiff39", "neighborDistanceMidK20", "k1", "periodWeekDensity11", 
  "periodWeekDensity14", "periodDensity9", "meanTimeDiff34", "meanTimeDiff61", 
  "k63", "midK1", "periodWeekDensity6", "weekBackDensity5", "periodDensity19", 
  "meanTimeDiff29", "midK26", "k11", "midK8", "meanTimeDiff60", 
  "periodDensity23", "periodWeekDensity5", "midK2", "weekBackDensity73", 
  "periodWeekDensity10", "periodWeekDensity13", "k16", "periodWeekDensity12", 
  "neighborDistanceMidK35", "periodWeekDensity9", "periodWeekDensity8", 
  "meanTimeDiff37", "meanTimeDiff23", "k31", "periodDensity24", 
  "neighborDistanceMidK31", "midK9", "meanTimeDiff33", "meanTimeDiff53", 
  "midK29", "isAcGroup2", "midK32", "meanTimeDiff50", "k23", "midK15", 
  "k2", "k6", "midK3", "midK28", "midK5", "midK38", "k26", "k34", 
  "k35", "midK33", "midK34")

# Inspect the least important predictors
leastImportantPredictors <- setdiff(allPredictors, top285Predictors)

# Add to bottom predictors - manual judgement
addToBottom <- c("periodDensity8"
                 , "periodDensity9"
                 , "periodDensity13"
                 , "periodDensity23"
)

# Add to top predictors
addToTop <- c("periodWeekDensity7"
              , "periodDensity16"
              , "periodDensity17"
              , "periodDensity18"
)

# Set the top and bottom predictors
topPredictors <- c(top285Predictors[!top285Predictors %in% addToBottom],
                   addToTop)
bottomPredictors <- c(addToBottom,
                      leastImportantPredictors[!leastImportantPredictors %in%
                                                 addToTop]
)

# Set the order of the predictors
orderedPredictors <- c(topPredictors, bottomPredictors)

# Check that all predictors are ordered
setdiff(allPredictors, orderedPredictors)

# Write the ordered predictors to a data file
saveRDS(orderedPredictors, saveFile)

# Drop features with missing values and save the ordered features to disk
missingValFeatures <-
  c("week52Correlation", "week53Correlation", "weekBackDensity21", 
    "weekBackDensity19", "weekBackDensity17", "weekBackDensity15", 
    "weekBackDensity13", "weekBackDensity11", "weekBackDensity9", 
    "weekBackDensity7", "weekBackDensity5", "weekBackDensity3",
    "weekBackDensity1", "weekBackDensity75", "weekBackDensity73",
    "weekBackDensity71", "weekBackDensity69", "weekBackDensity67",
    "weekBackDensity65", "weekBackDensity63", "weekBackDensity61",
    "weekBackDensity59"
  )
saveRDS(setdiff(orderedPredictors, missingValFeatures), saveFileNoMissing)