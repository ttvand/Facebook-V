# Function that returns the top N locations of a set of test records given a
# summary of the training data

# Target use:
# - Optimized for top 100 extraction: getTopKNN1
# - Optimized for top X (X<<100) extraction: getTopKNN2

# Functions that returns the top N classes based on the maximum K nearest
# neighbors calculation. Other K counts for the top N classes are returned as 
# well
getTopKNNDT1 <- function(testBatch, Ks, distanceConstants, topN, targetFolder){
  # Calculate the number of test records
  nbTest <- nrow(testBatch)
  
  # Find the maximum K and the number of considered Ks
  maxK <- max(Ks)
  nbKs <- length(Ks)
  
  # Extract the constant to rescale the X distance
  distanceConstantX <- distanceConstants[1]
  distanceId <- which(sqrt(distanceConstantX)==c(1,2.5,4,5.5,7,12,30))
  
  # Obtain the desired blocks sizes from the target folder
  xBlockSize <- c(0.2,0.25,0.5,0.5,0.4,0.5,1)[distanceId] #0.5 #1 #2
  nbXBlocks <- 10/xBlockSize - 1
  yBlockSize <- c(0.2,0.1,0.1,0.1,0.05,0.04,0.025)[distanceId] #0.04 #0.05 #0.1 #0.4 #1
  nbYBlocks <- 10/yBlockSize - 1
  nbBlocks <- nbXBlocks*nbYBlocks
  
  # Iterate over overlapping blocks of data and extract the nearest neighbors
  nearestNeighbors <-
    foreach(blockId=1:nbBlocks, .packages = c('data.table', 'fastOrdeR', 'bit64')
    ) %dopar% {
      # Calculate the considered spatial block and load it from a file
      xStart <- xBlockSize * ((blockId-1) %% nbXBlocks)
      yStart <- floor((blockId-1)/nbXBlocks)*yBlockSize
      
      block <- readRDS(paste0("../Data/", targetFolder, "/",
                              paste0(nbBlocks, " - ", xStart, " - ", yStart),
                              ".rds"))
      
      # Detect ids of testBatch that belong to the block
      xMin <- ifelse(xStart==0,-1, xStart + 1/2*xBlockSize)
      xMax <- ifelse((blockId %% nbXBlocks)==0, 10, xStart + 3/2*xBlockSize) + 1e-6
      yMin <- ifelse(yStart==0,-1, yStart + 1/2*yBlockSize)
      yMax <- ifelse(yStart>(10-(2.01*yBlockSize)), 10, yStart + 3/2*yBlockSize) + 1e-6
      
      # Find the ids that should be matched using the considered block
      blockIds <- which(testBatch$x > xMin & testBatch$x <= xMax &
                          testBatch$y > yMin & testBatch$y <= yMax)
      
      nbBatchIds <- length(blockIds)
      nbBlockIds <- nrow(block)
      
      # Return if no batches to process in the considered block
      if(nbBatchIds<1){
        out <- NULL
      } else{
        
        # Calculate the ids for the distance comparison and calculate the
        # rescaled distance between the considered ids
        distTable <- data.table(batchIdsRep=rep(blockIds,each=nbBlockIds),
                                blockIdsRep=rep(1:nbBlockIds,nbBatchIds))
        distTable[,squaredDistance:=
                    (testBatch[batchIdsRep,x]-block[blockIdsRep,x])^2 /
                    distanceConstants[1] + 
                    (testBatch[batchIdsRep,y]-block[blockIdsRep,y])^2 / 
                    distanceConstants[2]]
        
        # Calculate the time difference (minutes) between the observations
        distTable[,timeDiff:=(testBatch[batchIdsRep,time]-
                                block[blockIdsRep,time])]
        
        # Add the place ids of the neighbor blocks
        distTable[,place_id:=block[blockIdsRep,place_id]]
        
        # Extract the nearest neighbor blocks
        nearestNeighborsBlock <-
          lapply(blockIds, function(blockId){
            # Order distances and get according places
            distOrderIds <-
              fastOrder(distTable[batchIdsRep==blockId,squaredDistance])[1:maxK]
            
            # Output memory allocation
            out <- rep(NA_integer64_, topN*(1+nbKs*2) + nbKs)
            
            # First K calculation (add place id to output)
            distOrderPlaceTable <- distTable[batchIdsRep==blockId][
              distOrderIds[1:Ks[1]],list(.N,meanTimeDiff=mean(timeDiff)),
              by=place_id][order(-N,meanTimeDiff)]
            
            topPlaces <- min(c(topN,nrow(distOrderPlaceTable)))
            out[1:topPlaces] <- distOrderPlaceTable[1:topPlaces, place_id]
            out[topN + (1:topPlaces)] <-
              as.integer64(distOrderPlaceTable[1:topPlaces, N])
            out[topN*(1+nbKs) + (1:topPlaces)] <-
              as.integer64(distOrderPlaceTable[1:topPlaces, meanTimeDiff])
            
            # Rescaled distance to the max Kth nearest neighbor
            out[2*topN*(nbKs+1/2) + 1] <- as.integer64(
              distTable[batchIdsRep==blockId,squaredDistance][
                distOrderIds[maxK]] * 1e10)
            
            # Assign counts to out for other considered Ks
            if(nbKs>1){
              for(i in 2:nbKs){
                K <- Ks[i]
                distOrderPlaceTable <- distTable[batchIdsRep==blockId][
                  distOrderIds[1:K],list(.N,meanTimeDiff=mean(timeDiff)),
                  by=place_id][order(-N,meanTimeDiff)]
                
                # Match the top places to the highest K count neighbor counts
                matchTopK <- match(distOrderPlaceTable[1:topPlaces, place_id],
                                   out[1:topPlaces])
                matchTopKIds <- !is.na(matchTopK)
                matchTopK <- matchTopK[matchTopKIds]
                
                # Extract the neighbor counts and set missing match counts to 0
                topCounts <- as.integer64(distOrderPlaceTable[1:topPlaces, N])
                topCounts <- topCounts[matchTopKIds]
                topCounts[is.na(topCounts)] <- 0
                out[(i*topN) + (1:topN)] <- 0
                out[(i*topN) + matchTopK] <- topCounts
                
                # Extract mean time difference and set missing match time to
                # the mean time difference of the highest K count
                topMeanTimeDiff <-
                  as.integer64(distOrderPlaceTable[1:topPlaces, meanTimeDiff])
                topMeanTimeDiff <- topMeanTimeDiff[matchTopKIds]
                # if(any(is.na(topMeanTimeDiff))) browser()
                topMeanTimeDiff[is.na(topMeanTimeDiff)] <-
                  out[((1+nbKs)*topN) + (1:topN)][is.na(topMeanTimeDiff)]
                out[((i+nbKs)*topN) + (1:topN)] <-
                  out[((1+nbKs)*topN) + (1:topN)]
                out[((i+nbKs)*topN) + matchTopK] <- topMeanTimeDiff
                
                # Rescaled distance to the Kth nearest neighbor 
                out[2*topN*(nbKs+1/2) + i] <- as.integer64(
                  distTable[batchIdsRep==blockId,squaredDistance][
                    distOrderIds[K]] * 1e10)
              }
            }
            
            out
          })
        nearestNeighborsBlock <- do.call(rbind, nearestNeighborsBlock)
        out <- cbind(testBatch[blockIds,row_id],nearestNeighborsBlock)
      }
      
      out
    }
  
  # Convert to a matrix
  combined <- do.call(rbind, nearestNeighbors)
  
  # Restore original row ordering and drop row column
  targetIds <- match(as.integer(testBatch$row_id), as.integer(combined[,1]))
  combined[targetIds,-1]
}
