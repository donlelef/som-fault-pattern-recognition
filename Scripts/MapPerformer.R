# This script loads the wafer data from a file, applies the Kernel Density Extimation
# to the wafer and then the SOM classification. The results are plotted.

# Import
source(file = "Scripts/Initializer.R")

# Load data
dataFrame = read.csv2(file = "Data/Temporary Data/LOTTO1_DEF.csv")
KDEFrame = getKDEData(dataFrame = dataFrame)
waferIDs = getUniqueColumns(dataFrame = dataFrame, columns = c("LOT", "WAFER", "LAYER"))
features = getWaferFeatures(dataFrame = dataFrame)

# Iniatilize random seed and others parameters.
seed = 12 # for reproducibility
palette = rev(brewer.pal(11, "RdYlBu"))
grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
distributions = matrix(data = 0, nrow = nrow(waferIDs), ncol = length(grid$x)*length(grid$y))
row = 1

for(id in unique(KDEFrame$ID)){

  thisWaferData = subset(x = KDEFrame, subset = KDEFrame$ID == id)

  waferKDE = waferKDEMap(features = features, coords = thisWaferData[ , 2:3])
  waferKDE = bindCircularMap(rectangularMap = waferKDE, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay, outValue = NA)
  matrixPlot(title = "Estimated density function", sub = bquote("Lotto: "~.(as.character(waferIDs[row, ]$LOT))~"  Wafer: "~.(waferIDs[row, ]$WAFER)~"  Layer: "~.(waferIDs[row, ]$LAYER)),
             matrix = waferKDE, colorMap = rev(brewer.pal(11, "RdYlBu")))

  waferKDE = bindCircularMap(rectangularMap = waferKDE, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay, outValue = 0)
  distributions[row, ] = as.vector(waferKDE)

  row = row + 1

}

for(iterations in c(1,10,100)){
  set.seed(seed)
  waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = iterations)
  kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay)
}

# Plot the training process
oldPar = par()
par(pty = "m")
plot(x = waferSom, type = "changes", palette.name = rainbow)
plot(x = waferSom, type = "quality", palette.name = rainbow)
plot(x = waferSom, type = "count", palette.name = rainbow)
par(oldPar)
