# Functions that returns mid nearest neighbor features
#   # Counts for the nearest classes for different Ks and distance constants
midKnnCalc <- function(testBatch, Ks, distanceConstants, topN, targetFolder){
  # Calculate the number of test records
  nbTest <- nrow(testBatch)
  
  # Calculate the number of distance constants
  nbDistanceConstants <- length(distanceConstants)
  
  # Find the maximum K and the number of considered Ks
  maxK <- max(Ks)
  nbKs <- length(Ks)
  
  # Obtain the desired blocks sizes from the target folder
  xBlockSize <- 0.25
  nbXBlocks <- 10/xBlockSize - 1
  yBlockSize <- 0.1
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
        blockOut <- NULL
      } else{
        blockOut <- NULL
        for(i in 1:nbDistanceConstants){
          # Loop over the different distance constants and calculate the 
          # different nearest neighbor features
          
          # Calculate the ids for the distance comparison and calculate the
          # rescaled distance between the considered ids
          distTable <- data.table(batchIdsRep=rep(blockIds,each=nbBlockIds),
                                  blockIdsRep=rep(1:nbBlockIds,nbBatchIds))
          distTable[,squaredDistance:=
                      (testBatch[batchIdsRep,x]-block[blockIdsRep,x])^2 /
                      distanceConstants[i] + 
                      (testBatch[batchIdsRep,y]-block[blockIdsRep,y])^2]
          
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
              out <- rep(NA_integer64_, topN*(1+nbKs) + nbKs)
              
              # First K calculation (add place id to output)
              distOrderPlaceTable <- distTable[batchIdsRep==blockId][
                distOrderIds[1:Ks[1]],list(.N,meanTimeDiff=mean(timeDiff)),
                by=place_id][order(-N,meanTimeDiff)]
              
              topPlaces <- min(c(topN,nrow(distOrderPlaceTable)))
              out[1:topPlaces] <- distOrderPlaceTable[1:topPlaces, place_id]
              out[topN + (1:topPlaces)] <-
                as.integer64(distOrderPlaceTable[1:topPlaces, N])
              
              # Rescaled distance to the max Kth nearest neighbor
              out[topN*(1+nbKs) + 1] <- as.integer64(
                distTable[batchIdsRep==blockId,squaredDistance][
                  distOrderIds[maxK]] * 1e10)
              
              # Assign counts to out for other considered Ks
              if(nbKs>1){
                for(j in 2:nbKs){
                  K <- Ks[j]
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
                  out[(j*topN) + (1:topN)] <- 0
                  out[(j*topN) + matchTopK] <- topCounts
                  
                  # Rescaled distance to the Kth nearest neighbor 
                  out[topN*(1+nbKs) + j] <- as.integer64(
                    distTable[batchIdsRep==blockId,squaredDistance][
                      distOrderIds[K]] * 1e10)
                }
              }
              
              out
            })
          nearestNeighborsBlock <- do.call(rbind, nearestNeighborsBlock)
          out <- cbind(testBatch[blockIds,row_id],
                       sqrt(distanceConstants[i])*100,
                       nearestNeighborsBlock)
          blockOut <- rbind(blockOut, out)
        }
        
        # Order blockOut by row id
        blockOut <- blockOut[order(blockOut[,1]),]
      }
      
      blockOut
    }
  
  # Convert to a matrix
  combined <- do.call(rbind, nearestNeighbors)
  
  # Restore original row ordering and drop row column
  targetIds <- match(as.integer(testBatch$row_id), as.integer(combined[,1]))
  targetIdsRep <- rep(targetIds, each = nbDistanceConstants) + 0:7
  combined[targetIdsRep,-1]
}