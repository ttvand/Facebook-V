# Copy files to a backup folder after a specified time
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Submission/06-07-2016")

# Target folder
targetFolder <- "Predictions"

# Backup folder
backupFolder <- "predictions BU"

# Wait for a specified hour range
timeRange <- c(13,13)

# Wait for a valid time range
while(TRUE){
  exit <- as.numeric(format(Sys.time(),"%H")) >= timeRange[1] &&
    as.numeric(format(Sys.time(),"%H")) <= timeRange[2]
  
  if(exit){
    break
  }
  
  # Display sleep message
  cat("Waiting for a valid time range", timeRange, "\n")
  
  # Sleep since the features file is not available
  Sys.sleep(300) 
}

# Copy the predictions files
file.copy(from=paste0(getwd(), "/", targetFolder, "/",
                      list.files(file.path(getwd(), targetFolder))),
          to=file.path(getwd(), backupFolder), 
          overwrite = TRUE)