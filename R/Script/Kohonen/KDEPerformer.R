# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and plots the result.

# Initialize distributions
source(file = "Script//initializer.R")
library(ks)
library(kohonen)

# Load data
waferData = readRDS(file = "simulatedWafers.rds")
# bandwidthData = readRDS(file = "Data//maxLikelihoodBestBandwidths.rds")
# fittedFaults = bandwidthData$faults
# fittedBand = bandwidthData$bestBand
waferData = unique.data.frame(x = waferData)
waferNames = unique(x = waferData$wafer)

# Initializations
distributions = matrix(data = 0, nrow = length(waferNames), ncol = length(distributionsList[[1]]))
row = 1

# Perform KDE
for(wafer in waferNames){
  
  thisWaferDefects = waferData[waferData$wafer == wafer, ]
  faultPositions = as.matrix.data.frame(thisWaferDefects[ , 2:3])
#   bandwidth = bestBandwidth(fittedBandwidth = fittedBand, fittedFaults = fittedFaults, faults = ncol(faultPositions))
#   estimation = bkde2D(faultPositions, bandwidth = bandwidth, 
#                           range.x = list( c(min(grid$x), max(grid$x)), c(min(grid$y), max(grid$y))), 
#                           gridsize = c(length(grid$x), length(grid$y)))
  estimation = kde(x = faultPositions, 
                     gridsize = c(length(grid$x), length(grid$y)), 
                     xmin = c(min(grid$x), min(grid$y)), 
                     xmax = c(max(grid$x), max(grid$y)))
  
  # Plot every single wafer 
  extimatedFunction = bindCircularMap(rectangularMap = estimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
  matrixPlot(title = "Extimated density function", matrix = extimatedFunction, colorMap = rainbow(20))
  
  # Preparing distributions for SOM
  extimatedFunction = bindCircularMap(rectangularMap = estimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = 0)
  distributions[row, ] = as.vector(extimatedFunction)
  row = row + 1

}

waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"))

oldPar = par()
par(mfrow=c(2,2), mar = c(1,1,1,1))
for(i in 1:nrow(waferSom$codes)){
  wafer = matrix(data = waferSom$codes[i, ], nrow = nrow(distributionsList[[1]]), ncol = ncol(distributionsList[[1]]))
  wafer = bindCircularMap(rectangularMap = wafer, waferRay = ray, dieWidth = dieWidth, dieHeight = dieHeight, outValue = NA)
  matrixPlot(title = "", matrix = wafer, colorMap = rainbow(20))  
}
par(oldPar)

