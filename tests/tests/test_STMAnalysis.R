data = read.csv2(file = "tests/testData/testFile.csv", dec = ".")
layerData = getLayerData(dataFrame = data, lot = "LOTTO1", wafer = 1, layer = 1)
features = getWaferFeatures(dataFrame = layerData)
KDELayer = getKDEData(dataFrame = layerData)

test.waferFaultMap = function(){

  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  faultIndex = indexFromCoord(coords = as.matrix(KDELayer[ , 2:3]), features = features, grid = grid)
  waferMap = waferFaultMap(features = features, index = faultIndex)
  checkEquals(2, length(which(waferMap == 1)))

}

test.waferKDEMap = function(){

  KDEFrame = getKDEData(dataFrame = data)
  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  waferMap = waferKDEMap(features = features, coords = KDEFrame[ , 2:3])
  checkEquals(length(grid$x), nrow(waferMap))
  checkEquals(length(grid$y), ncol(waferMap))

}

test.distributions = function(){
  STMData = read.csv2("Data/Temporary Data/LOTTO1_DEF.csv")
  KDEDataFrame = getKDEData(dataFrame = STMData)
  feat = getWaferFeatures(dataFrame = STMData)
  distributions = distributions(KDEdataFrame = KDEDataFrame, features = feat)
  checkEquals(103, nrow(distributions))
}
