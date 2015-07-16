# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and plots the result.

# Initialize distributions
source(file = "Script//initializer.R")
library(ks)
library(kohonen)
library(SOMWaferClassification)

# Iniatilize random seed.
seed = 12 # for reproducibility

# Load data
waferData = readRDS(file = "Data//TemporaryData/simulatedWafersLong.rds")
waferData = unique.data.frame(x = waferData)

# Perfarm KDE
gird = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
distributions = KDEOnWafer(dataFrame = waferData, grid = grid,
           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, 
           plotDistributions = TRUE, colorMap = palette)

# Plot the real distributions
oldPar = par()
par(new = FALSE, mfrow=c(2,2), mar = c(1,1,1,1))
for(distribution in distributionsList){
  distribution = bindCircularMap(rectangularMap = distribution, waferRay = ray, dieWidth = dieWidth, dieHeight = dieHeight, outValue = NA)
  matrixPlot(title = "", matrix = distribution, colorMap = palette)
}
par(oldPar)

# Plot the SOM code vector after 1 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 1)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)

# Plot the SOM code vector after 10 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 10)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)


# Plot the SOM code vector after 100 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 2, ydim = 2, topo = "rectangular"), rlen = 100) 
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)


# Plot the training process
oldPar = par()
par(pty = "m")
plot(x = waferSom, type = "changes", palette.name = rainbow)
plot(x = waferSom, type = "quality", palette.name = rainbow)
par(oldPar)

# Map a new data set using the trained network
waferPerDistribution = c(spot = 5, ring = 4, threeSpot = 3, uniform = 2)
defectsRange = c(30, 60)
newDataFrame = generateWaferFormDistribution(distributionsList = distributionsList, 
                                        waferPerDistribution = waferPerDistribution, 
                                        defectsRange = defectsRange, 
                                        dieWidth = dieWidth, dieHeight = dieHeight)
newDataDistributions = KDEOnWafer(dataFrame = newDataFrame, grid = grid,
                                     dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, 
                                     plotDistributions = TRUE, colorMap = palette)

newSom = map(x = waferSom, newdata = newDataDistributions)
classifiedWafer = c(spot = sum(newSom$unit.classif == 1), 
                    ring = sum(newSom$unit.classif == 4), 
                    threeSpot = sum(newSom$unit.classif == 2),
                    uniform = sum(newSom$unit.classif == 3))

# Plot mapping with pchs
plot(x = waferSom, type = "mapping", classif = newSom$unit.classif, pchs = newSom$unit.classif)
legend(x = "topright", legend = names(waferPerDistribution), pch = unique(newSom$unit.classif))

# Plot mapping with pie chart
pie(x = waferPerDistribution, labels = names(x = waferPerDistribution), col = rainbow(4), main = "Generated wafer distribution")
pie(x = classifiedWafer, labels = names(x = classifiedWafer), col = rainbow(4), main = "SOM classification for wafer distribution")

# Plot mapping with one hot
i = 1
for(waferID in unique(newDataFrame$wafer)){
  thisWaferDistribution = matrix(newDataDistributions[i, ], nrow = 1)
  classification = map(x = waferSom, newdata = thisWaferDistribution)$unit.classif
  colors = rep(x = "white", times = nrow(waferSom$codes))
  colors[classification] = "red"
  plot(x = waferSom, type = "mapping", classif = classification, main = paste("Wafer mapping", waferID, sep = " "), bgcol = colors)
  i = i + 1
}

