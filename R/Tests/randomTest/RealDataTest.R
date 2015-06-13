# Test for loading data from file

library(gdata)

# dataFrame = read.xls(xls = "STMData/LOTTO1_RIDOTTISSIMO.xlsx", verbose = TRUE)
wafers = unique(dataFrame$WAFER)

for(wafer in wafers){
  
  waferData = dataFrame[dataFrame$WAFER == wafer, ]
  grid = prepareWaferGrid(dieWidth = waferData$DIE_WIDTH[1], dieHeight = waferData$DIE_HEIGHT[1], waferRay = waferData$WAFER_SIZE[1]/2)
  STMDefectCoords = matrix(data = c(waferData$DIE_X, waferData$DIE_Y), ncol = 2, nrow = length(waferData$DIE_X))
  STMDefectCoords = unique(STMDefectCoords)
  STMDefectCoords[,1] = STMDefectCoords[,1] + length(grid$x)/2
  STMDefectCoords[,2] = STMDefectCoords[,2] + length(grid$y)/2
  waferMap = matrix(data = 0, nrow = length(grid$x), ncol = length(grid$y))
  for(i in 1:nrow(STMDefectCoords)){
    waferMap[STMDefectCoords[i,1], STMDefectCoords[i,2]] = 1
  }
  waferMap = bindCircularMap(rectangularMap = waferMap, dieWidth = waferData$DIE_WIDTH[1], dieHeight = waferData$DIE_HEIGHT[1], waferRay = waferData$WAFER_SIZE[1]/2, outValue = NA)
  matrixPlot(title = "Fault Distribution", matrix = waferMap, colorMap = rainbow(2))
}
