# This script loads the original data from a RDS file, plots the fault maps and then 
# save on a file the data required for the following SOM clustering.

# Import
source(file = "Scripts/Initializer.R")

# Initializations
dataFrame = read.csv2(file = "Data/Temporary Data/LOTTO3.csv")
wafersID = getUniqueColumns(dataFrame = dataFrame, columns = c("LOT", "WAFER", "LAYER"))

for(i in 1:nrow(wafersID)){
  
  # Extract data for the single wafer
  waferData = getLayerData(dataFrame = dataFrame, lot = wafersID[i, ]$LOT, wafer = wafersID[i, ]$WAFER, layer = wafersID[i, ]$LAYER)
  features = getWaferFeatures(dataFrame = waferData)
  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  kdeFrame = getKDEData(dataFrame = waferData)
  index = indexFromCoord(coords = kdeFrame[ ,2:3], features = features, grid = grid)
  
  # Get faulty chip position and place it on the wafer
  waferMap = waferFaultMap(features = features, index = index, faultValue = 1, nonFaultValue = 0, outValue = NA)
  matrixPlot(title = "Fault Distribution", 
             sub = bquote("Lotto: "~.(as.character(wafersID[i, ]$LOT))~
                          "  Wafer: "~.(wafersID[i, ]$WAFER)~
                          "  Layer: "~.(wafersID[i, ]$LAYER)),  
             matrix = waferMap, colorMap = c("blue", "red"))
  
  # Find defects coordiantes and launch KDE
  extimatedFunction = waferKDEMap(features = features, coords = kdeFrame[ , 2:3])
  extimatedFunction = bindCircularMap(rectangularMap = extimatedFunction, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay, outValue = NA)
  matrixPlot(title = "Estimated density function", 
             sub = bquote("Lotto: "~.(as.character(wafersID[i, ]$LOT))~
                            "  Wafer: "~.(wafersID[i, ]$WAFER)~
                            "  Layer: "~.(wafersID[i, ]$LAYER)),
             matrix = extimatedFunction, colorMap = rev(brewer.pal(11, "RdYlBu")))
}




