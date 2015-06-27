# Test for loading data from file

library(KDEPlotTools)
library(KDEFaultPattern)
library(STMWrapper)
library(RColorBrewer)
library(ks)

# Initializations
dataFrame = readRDS(file = "Data//STMData/lotto1.rds")
wafersID = dataFrame[ , c("LOT", "WAFER", "LAYER")]
wafersID = unique(wafersID)
waferNames = vector(mode = "character")
faultX = vector(mode = "numeric")
faultY = vector(mode = "numeric")

for(i in 1:nrow(wafersID)){
  
  # Extract data for the single wafer
  waferData = dataFrame[dataFrame$LOT == wafersID[i, ]$LOT & dataFrame$WAFER == wafersID[i, ]$WAFER & dataFrame$LAYER == wafersID[i, ]$LAYER, ]
  dieWidth = waferData$DIE_WIDTH[1]
  dieHeight = waferData$DIE_HEIGHT[1]
  waferRay = waferData$WAFER_SIZE[1]/2
  alignmentX = waferData$ALIGNMENT_X[1]
  alignmentY = waferData$ALIGNMENT_Y[1]
  
  # ???
  if(alignmentY < -dieHeight){
    alignmentY = alignmentY + 2*dieHeight
  }
  
  originalDieCenterX = alignmentX + dieWidth/2
  originalDieCenterY = alignmentY + dieHeight/2
  
  # Get faulty chip position and place it on the wafer
  grid = createSTMGrid(waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY)
  STMDefectCoords = getLogicalCoordsFromData(waferData = waferData)
  shiftedCoords = shiftReference(logicalCoords = STMDefectCoords, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY, grid = grid)
  waferMap = fillMatrixFromIndex(logicalCoords = shiftedCoords, matchingValue = 1, nonMatchingValue = 0, nrow = length(grid$x), ncol = length(grid$y))
  waferMap = bindCircularMap(rectangularMap = waferMap, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, alignmentX = originalDieCenterX, alignmentY = originalDieCenterY, outValue = NA)
  matrixPlot(title = "Fault Distribution", 
             sub = bquote("Lotto: "~.(as.character(wafersID[i, ]$LOT))~
                          "  Wafer: "~.(wafersID[i, ]$WAFER)~
                          "  Layer: "~.(wafersID[i, ]$LAYER)),  
             matrix = waferMap, colorMap = c("green", "red"))
  
  # Find defects coordiantes and launch KDE
  dieCenterDefectCoords = getCoordsFromLogical(logicalCoords = STMDefectCoords, dieWidth = dieWidth, dieHeigth = dieHeight, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY)
  extimation = kde(x = dieCenterDefectCoords, gridsize = c(length(grid$x), length(grid$y)), 
                   xmin = c(min(grid$x), min(grid$y)), xmax = c(max(grid$x), max(grid$y)))
  extimatedFunction = bindCircularMap(rectangularMap = extimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, alignmentX = originalDieCenterX, alignmentY = originalDieCenterY, outValue = NA)
  matrixPlot(title = "Extimated density function", 
             sub = bquote("Lotto: "~.(as.character(wafersID[i, ]$LOT))~
                            "  Wafer: "~.(wafersID[i, ]$WAFER)~
                            "  Layer: "~.(wafersID[i, ]$LAYER)),
             matrix = extimatedFunction, colorMap = rev(brewer.pal(11, "RdYlBu")))
  extimatedFunction = bindCircularMap(rectangularMap = extimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, alignmentX = originalDieCenterX, alignmentY = originalDieCenterY, outValue = 0)
  
  # Save data for KDE and SOM
  thisWaferName = paste(as.character(wafersID[i, ]$LOT), "/", wafersID[i, ]$WAFER, "/", wafersID[i, ]$LAYER, sep = "")
  thisWaferName = rep(thisWaferName, times = length(dieCenterDefectCoords[, 1]))
  faultX = c(faultX, dieCenterDefectCoords[, 1])
  faultY = c(faultY, dieCenterDefectCoords[, 2])
  waferNames = c(waferNames, thisWaferName)  
}

KDEData = data.frame(wafers = waferNames, faultX = faultX, faultY = faultY)
saveRDS(object = KDEData, file = "lotto1KDE.rds", ascii = TRUE)



