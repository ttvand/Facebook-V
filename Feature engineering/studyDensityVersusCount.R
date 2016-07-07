# Logic to check the relation between the maximum density to the Kth
# observation in a two-dimensional space

# Clear the workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Tom/Documents/Kaggle/Facebook/Feature engineering")

# Number of observations
nObs <- c(1e4,2e4)

# Considered K
K <- 1000

# Considered coordinate
coor <- c(5,5)

# Generate random points in the 0-10 grid
x1 <- 10*runif(nObs[1])
y1 <- 10*runif(nObs[1])
x2 <- 10*runif(nObs[2])
y2 <- 10*runif(nObs[2])

# Calculate the distances to the observation
distances1 <- (coor[1]-x1)^2 + (coor[2]-y1)^2
distances2 <- (coor[1]-x2)^2 + (coor[2]-y2)^2

# Order the distances
distanceOrder1 <- order(distances1)
distanceOrder2 <- order(distances2)

# Distance to the kth neighbor
maxNeighborDistance1 <- distances1[distanceOrder1[K]]
maxNeighborDistance2 <- distances2[distanceOrder2[K]]

cat(round(maxNeighborDistance1/maxNeighborDistance2,2))
