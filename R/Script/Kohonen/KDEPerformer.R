# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and plots the result.

# Initialize distributions
source(file = "Script//initializer.R")
library(ks)
library(kohonen)
library(SOMWaferClassification)

# Iniatilize random seed.
seed = 11 # for reproducibility

# Load data
waferData = readRDS(file = "simulatedWafers.rds")
waferData = unique.data.frame(x = waferData)

# Perfarm KDE
distributions = KDEOnWafer(dataFrame = waferData,
           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, 
           plotDistributions = FALSE)

# Plot the real distributions
oldPar = par()
par(new = FALSE, mfrow=c(2,2), mar = c(1,1,1,1))
for(distribution in distributionsList){
  distribution = bindCircularMap(rectangularMap = distribution, waferRay = ray, dieWidth = dieWidth, dieHeight = dieHeight, outValue = NA)
  matrixPlot(title = "", matrix = distribution, colorMap = rainbow(20))
}
par(oldPar)

# Plot the SOM code vector after one iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 1)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = rainbow(20), dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)

# Plot the SOM code vector after one iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 10)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = rainbow(20), dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)


# Plot the SOM code vector after one iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 100) 
kohonenCodesPlot(kohonenObject = waferSom, colorMap = rainbow(20), dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)


# Plot the training process
oldPar = par()
par(pty = "m")
plot(x = waferSom, type = "changes", palette.name = rainbow)
plot(x = waferSom, type = "quality", palette.name = rainbow)
par(oldPar)


