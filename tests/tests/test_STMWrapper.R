data = read.csv2(file = "tests/testData/testFile.csv", dec = ".")
layerData = getLayerData(dataFrame = data, lot = "LOTTO1", wafer = 1, layer = 1)
features = getWaferFeatures(dataFrame = layerData)
KDELayer = getKDEData(dataFrame = layerData)

test.createSTMGrid = function(){
  
  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  checkEquals(2, length(grid))
  checkTrue(min(grid$x) >= - features$waferRay)
  checkTrue(min(grid$y) >= - features$waferRay)
  checkTrue(max(grid$x) <= features$waferRay)
  checkTrue(max(grid$y) <= features$waferRay)
  for(i in 2:length(grid$x)){
    checkEquals(features$dieWidth, grid$x[i] - grid$x[i-1])
  }
  for(i in 2:length(grid$y)){
    checkEquals(features$dieHeight, grid$y[i] - grid$y[i-1])
  }
}

test.indexFromCoord = function(){
  
  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  faultIndex = indexFromCoord(coords = as.matrix(KDELayer[ , 2:3]), features = features, grid = grid)
  checkEquals(2, ncol(faultIndex))
  checkEquals(nrow(KDELayer), ncol(faultIndex))
  checkEquals(52, faultIndex[1, 1])
  checkEquals(27, faultIndex[1, 2])
  checkEquals(54, faultIndex[2, 1])
  checkEquals(29, faultIndex[2, 2])
  
}

test.fillMatrixFromIndex = function(){
  
  index = cbind(c(1,2), c(1,2))
  matrix = fillMatrixFromIndex(index = index, matchingValue = 1, nonMatchingValue = 0, nrow = 2, ncol = 2)
  checkEquals(matrix(c(1,0,0,1), nrow = 2), matrix)
  
}






