# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and plots the result.

# Initialize distributions
source(file = "Script//initializer.R")
library(kohonen)
library(animation)
library(SOMWaferClassification)

# Iniatilize parameters.
seed = 11 # for reproducibility
iterations = 1:100

# Load data
waferData = readRDS(file = "simulatedWafers.rds")
waferData = unique.data.frame(x = waferData)

# Perfarm KDE
distributions = KDEOnWafer(dataFrame = waferData,
                           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, 
                           plotDistributions = FALSE)


# Plot the SOM code vector after one iteration
set.seed(seed)
animatedPlot = function(){
  for(iteration in iterations){
    waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = iteration) 
    kohonenCodesPlot(kohonenObject = waferSom, colorMap = rainbow(20), dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)  
  }  
}
ani.options(interval = 0.4, nmax = max(iterations))
saveGIF(expr = animatedPlot(), clean = TRUE)

