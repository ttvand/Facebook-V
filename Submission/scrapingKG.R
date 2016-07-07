# Clear the workspace
rm(list=ls())

# Load the required libraries
library(rvest)
library(ggplot2)

# Set the target page
targetPage <- "https://www.kaggle.com/c/facebook-v-predicting-check-ins/leaderboard"

# Scrape target page
page <- read_html(targetPage)

# Extract the ranking
rankingList <- page %>%
  html_table()
ranking <- rankingList[[1]][,-2]

# Calculate the days since the best submission
names(ranking)[5] <- "timeString"
ranking$lastSubmTime <-
  gsub("^.*[,] |\\(.*$|\\\r.*", "", ranking$timeString)
ranking$lastSubmTime <- as.POSIXct(ranking$lastSubmTime,
                                   format="%d %b %Y %H:%M:%S")
ranking$delayTime <- 0
delayIds <- grepl("\\(", ranking$timeString)
ranking$delayTime[delayIds] <-
  as.numeric(gsub(".*\\(|\\).*|-|h|d", "", ranking$timeString[delayIds]))
ranking$daysSinceBestS <-
  as.numeric(difftime(Sys.time(), ranking$lastSubmTime, units="days")) +
  ifelse(!delayIds, 0,
         ifelse(substring(ranking$timeString,
                          nchar(ranking$timeString)-1) == "d)",
                ranking$delayTime, ranking$delayTime/24))-1/12

# Calculate the projected score: best score + (d till end - d best score)/1000
daysLeft <-
  as.numeric(difftime(as.POSIXct("7 7 2016", format = "%d %m %Y", tz = "UTC"),
                      Sys.time(), units = "days"))
ranking$projectedS <-
  100*ranking$Score + (daysLeft + ranking$daysSinceBestS)/10 - 
  (as.numeric(rownames(ranking))>100)

# Trim team names
ranking$`Team Name` <- substring(ranking$`Team Name`,1,20)

# Plot the ranking as a bar chart
ranking$`Team Name` <- factor(ranking$`Team Name`,
                              levels = ranking$`Team Name`[order(ranking$Score,
                                                                 decreasing = TRUE)])
ranking$ScoreText <- as.character(round(ranking$Score*100,2))
p <- ggplot(ranking[1:15,], aes(x = `Team Name`,
                                y = Score,
                                fill = `Team Name`,
                                label = ScoreText)) + 
  geom_hline(aes(yintercept=ranking$Score[10]), colour="red") +
  # geom_hline(aes(yintercept=0.6211), colour="black") +
  # geom_hline(aes(yintercept=0.62289), colour="darkgreen") +
  geom_bar(stat = "identity") +
  geom_text(vjust=-0.25) + 
  coord_cartesian(ylim=c(0.600,0.623)) +
  theme(legend.position="none",
        axis.text.x=element_text(angle = -90, vjust = 0.4, hjust = 1))
print(p)
