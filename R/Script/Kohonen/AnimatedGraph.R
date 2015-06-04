# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and plots the result.

# Initialize distributions
source(file = "Script//initializer.R")
library(kohonen)
library(animation)
library(SOMWaferClassification)

# Iniatilize parameters.
seed = 11 # for reproducibility
iterations = 1:20

# Load data
waferData = readRDS(file = "Data//simulatedWafers.rds")
waferData = unique.data.frame(x = waferData)

# Perfarm KDE
distributions = KDEOnWafer(dataFrame = waferData,
                           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, 
                           plotDistributions = FALSE)


# Plot the SOM code vector after one iteration
set.seed(seed)
animatedPlot = function(){
  for(iteration in iterations){
    waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = iteration, toroidal = FALSE) 
    kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)  
  }  
}
ani.options(interval = 0.4, nmax = max(iterations))
saveGIF(expr = animatedPlot(), clean = TRUE)

