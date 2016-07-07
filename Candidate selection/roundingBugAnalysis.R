# Set the number of distance ids (fixed)
nbDistanceIds <- 7

# Logical vector to represent potential numerical rounding issues
roundingIssues <- vector(length = nbDistanceIds)

# Loop over the distance IDs
for(i in 1:nbDistanceIds){
  distanceId <- i
  
  # Generate block counts
  xBlockSize <- c(0.2,0.25,0.5,0.5,0.4,0.5,1)[distanceId] #0.5 #1 #2
  nbXBlocks <- 10/xBlockSize - 1
  yBlockSize <- c(0.2,0.1,0.1,0.1,0.05,0.04,0.025)[distanceId] #0.04 #0.05 #0.1 #0.4 #1
  nbYBlocks <- 10/yBlockSize - 1
  nbBlocks <- nbXBlocks*nbYBlocks
  
  # Check last block and signal difference issues
  blockId <- nbBlocks
  
  xStart <- xBlockSize * ((blockId-1) %% nbXBlocks)
  yStart <- floor((blockId-1)/nbXBlocks)*yBlockSize
  
  # Detect ids of testBatch that belong to the block
  xMin <- ifelse(xStart==0,-1, xStart + 1/2*xBlockSize)
  xMax <- ifelse(xStart==(10-2*xBlockSize), 10, xStart + 3/2*xBlockSize) + 1e-6
  yMin <- ifelse(yStart==0,-1, yStart + 1/2*yBlockSize)
  yMax <- ifelse(yStart==(10-2*yBlockSize), 10, yStart + 3/2*yBlockSize) + 1e-6
  
  roundingIssues[i] <- (xMax<10) || (yMax<10)
}

names(roundingIssues) <- c(1,2.5,4,5.5,7,12,30)
cat(roundingIssues)
which(roundingIssues)