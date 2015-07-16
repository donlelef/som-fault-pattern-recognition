# This script performs KDE and SOM and then writes the result in a file

# Import
library(kohonen)
library(SOMWaferClassification)
library(KDEFaultPattern)
library(STMWrapper)
library(KDEPlotTools)
library(RColorBrewer)

# Load data
palette = rev(brewer.pal(11, "RdYlBu"))
STMdataFrame = readRDS(file = "Data/STMData/lotto1.rds")
waferData = readRDS(file = "Data/TemporaryData/lotto1KDE.rds")
waferData = unique.data.frame(x = waferData)

# Iniatilize random seed.
seed = 12 # for reproducibility
waferRay = mean(STMdataFrame$WAFER_SIZE, na.rm = TRUE)/2
dieWidth = mean(STMdataFrame$DIE_WIDTH, na.rm = TRUE)
dieHeight = mean(STMdataFrame$DIE_HEIGHT, na.rm = TRUE)

# Perfarm KDE
grid = createSTMGrid(waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight)
distributions = KDEOnWafer(dataFrame = waferData, grid = grid, 
                           dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, 
                           plotDistributions = FALSE)

# Plot the SOM code vector after 100 iteration
set.seed(seed)
waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 100) 
kohonenCodesPlot(kohonenObject = waferSom, colorMap = palette, grid = grid, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)

recapFrame = writeClassificationRecap(faultPositionDataFrame = waferData, waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight, splitID = TRUE)
