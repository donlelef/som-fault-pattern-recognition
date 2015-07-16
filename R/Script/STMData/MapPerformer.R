# This script loads the wafer data from a file, applies the Kernel Density Extimation 
# to the wafer and then the SOM classification. The results are plotted.

# Initialize distributions
library(ks)
library(kohonen)
library(SOMWaferClassification)
library(RColorBrewer)
library(KDEFaultPattern)
library(STMWrapper)
library(KDEPlotTools)

# Load data
STMdataFrame = readRDS(file = "Data/STMData/lotto1.rds")
waferData = readRDS(file = "Data/TemporaryData/lotto1KDE.rds")
waferData = unique.data.frame(x = waferData)

# Iniatilize random seed.
seed = 12 # for reproducibility
palette = rev(brewer.pal(11, "RdYlBu"))
waferRay = mean(STMdataFrame$WAFER_SIZE, na.rm = TRUE)/2
dieWidth = mean(STMdataFrame$DIE_WIDTH, na.rm = TRUE)
dieHeight = mean(STMdataFrame$DIE_HEIGHT, na.rm = TRUE)

# Perfarm KDE
grid = createSTMGrid(waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight)
distributions = KDEOnWafer(dataFrame = waferData, grid = grid, 
                           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, 
                           plotDistributions = TRUE, colorMap = palette)

# Plot the SOM code vector after 1 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 1)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)

# Plot the SOM code vector after 10 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 10)
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)


# Plot the SOM code vector after 100 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 100) 
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)


# Plot the training process
oldPar = par()
par(pty = "m")
plot(x = waferSom, type = "changes", palette.name = rainbow)
plot(x = waferSom, type = "quality", palette.name = rainbow)
plot(x = waferSom, type = "count", palette.name = rainbow)
par(oldPar)

# Map each wafer to the correct unit
# and save the data in vectors
lot = wafer = layer = vector(mode = "character")
faults = cluster = vector(mode = "numeric")
i = 1
for(waferID in unique(waferData$wafers)){
  thisWaferData = waferData[waferData$wafers == waferID, ]
  thisWaferDistribution = matrix(distributions[i, ], nrow = 1)
  thisWaferClassification = map(x = waferSom, newdata = thisWaferDistribution)$unit.classif
  splittedID = unlist(strsplit(x = waferID, split = "[/]"))
  lot = c(lot, splittedID[1])
  wafer = c(wafer, splittedID[2])
  layer = c(layer, splittedID[3])
  faults = c(faults, nrow(thisWaferData))
  cluster = c(cluster, thisWaferClassification)
  colors = rep(x = "white", times = nrow(waferSom$codes))
  colors[thisWaferClassification] = "red"
  plot(x = waferSom, type = "mapping", classif = thisWaferClassification, main = paste("Wafer mapping", waferID, sep = " "), bgcol = colors)
  i = i + 1
}

# Save the data of each wafer in a file
waferRecap = data.frame(lot = lot, wafer = wafer, layer = layer, faults = faults, cluster = cluster)



