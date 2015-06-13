# Test for loading data from file

library(ks)
library(KDEPlotTools)
library(KDEFaultPattern)
library(RColorBrewer)
library(STMWrapper)

dataFrame = readExcel(xlsToRead = "STMData/LOTTO1_RIDOTTISSIMO.xlsx", saveOnRDS = TRUE)
wafers = unique(dataFrame$WAFER)

for(wafer in wafers){
  
  # Extract data
  waferData = dataFrame[dataFrame$WAFER == wafer, ]
  dieWidth = waferData$DIE_WIDTH[1]
  dieHeight = waferData$DIE_HEIGHT[1]
  waferRay = waferData$WAFER_SIZE[1]/2
  alignmentX = waferData$ALIGNMENT_X[1]
  alignmentY = waferData$ALIGNMENT_Y[1]
  originalDieCenterX = alignmentX + dieWidth/2
  originalDieCenterY = alignmentY + dieHeight/2
  
  # Get faulty chip position and place it on the wafer
  grid = createSTMGrid(waferRay = waferRay, dieWidth = dieWidth, dieHeigth = dieHeight, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY)
  STMDefectCoords = getLogicalFaultCoords(waferData)
  shiftedCoords = shiftReference(logicalCoords = STMDefectCoords, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY, grid = grid)
  waferMap = fillMatrixFromIndex(logicalCoords = shiftedCoords, matchingValue = 1, nonMatchingValue = 0, nrow = length(grid$x), ncol = length(grid$y))
  waferMap = bindCircularMap(rectangularMap = waferMap, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, alignmentX = originalDieCenterX, alignmentY = originalDieCenterY, outValue = NA)
  matrixPlot(title = "Fault Distribution", matrix = waferMap, colorMap = c("green", "red"))
  
  # Find defects coordiantes and launch KDE
  DieCenterDefectCoords = getCoordsFromLogical(logicalCoords = STMDefectCoords, dieWidth = dieWidth, dieHeigth = dieHeight, originalDieCenterX = originalDieCenterX, originalDieCenterY = originalDieCenterY)
  extimation = kde(x = DieCenterDefectCoords, gridsize = c(length(grid$x), length(grid$y)), 
                   xmin = c(min(grid$x), min(grid$y)), xmax = c(max(grid$x), max(grid$y)))
  extimatedFunction = bindCircularMap(rectangularMap = extimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, alignmentX = originalDieCenterX, alignmentY = originalDieCenterY, outValue = NA)
  matrixPlot(title = "Extimated density function", matrix = extimatedFunction, colorMap = rev(brewer.pal(11, "RdYlBu")))
  
}
