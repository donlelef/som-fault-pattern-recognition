context("STM data mining package tests")

test_that(desc = "getWaferFeature works", code = {

  lot1Data = getLotData(dataFrame = fileExample, lot = "LOTTO1")
  features = getWaferFeatures(dataFrame = lot1Data)
  expect_equal(expected = 3, object = length(features))
  expect_equal(expected = 10, object = features$dieWidth)
  expect_equal(expected = 20, object = features$dieHeight)
  expect_equal(expected = 500, object = features$waferRay)

})

test_that(desc = "getLotData works", code = {

  lot1Data = getLotData(dataFrame = fileExample, lot = "LOTTO1")
  expect_equal(expected = 12, object = nrow(lot1Data))
  expect_equal(expected = "LOTTO1", object = unique(as.character(lot1Data$LOT)))
  expect_equal(expected = ncol(fileExample), object = ncol(lot1Data))

  lot2Data = getLotData(dataFrame = fileExample, lot = "LOTTO2")
  expect_equal(expected = 4, object = nrow(lot2Data))
  expect_equal(expected = "LOTTO2", object = unique(as.character(lot2Data$LOT)))
  expect_equal(expected = ncol(fileExample), object = ncol(lot2Data))

})

test_that(desc = "getWaferData works", code = {

  waferData = getWaferData(dataFrame = fileExample, lot = "LOTTO1", wafer = 1)
  expect_equal(expected = 9, object = nrow(waferData))
  expect_equal(expected = "LOTTO1", object = unique(as.character(waferData$LOT)))
  expect_equal(expected = 1, object = unique(as.integer(waferData$WAFER)))
  expect_equal(expected = ncol(fileExample), object = ncol(waferData))

  waferData = getWaferData(dataFrame = fileExample, lot = "LOTTO1", wafer = 2)
  expect_equal(expected = 3, object = nrow(waferData))
  expect_equal(expected = "LOTTO1", object = unique(as.character(waferData$LOT)))
  expect_equal(expected = 2, object = unique(as.integer(waferData$WAFER)))

  waferData = getWaferData(dataFrame = fileExample, lot = "LOTTO2", wafer = 2)
  expect_equal(expected = 2, object = nrow(waferData))
  expect_equal(expected = "LOTTO2", object = unique(as.character(waferData$LOT)))
  expect_equal(expected = 2, object = unique(as.integer(waferData$WAFER)))

})

test_that(desc = "getLayerData works", code = {

  layerData = getLayerData(dataFrame = fileExample, lot = "LOTTO1", wafer = 1, layer = 1)
  expect_equal(expected = 3, object = nrow(layerData))
  expect_equal(expected = "LOTTO1", object = unique(as.character(layerData$LOT)))
  expect_equal(expected = 1, object = unique(as.integer(layerData$WAFER)))
  expect_equal(expected = ncol(fileExample), object = ncol(layerData))

  layerData = getLayerData(dataFrame = fileExample, lot = "LOTTO2", wafer = 1, layer = 1)
  expect_equal(expected = 1, object = nrow(layerData))
  expect_equal(expected = "LOTTO2", object = unique(as.character(layerData$LOT)))
  expect_equal(expected = 1, object = unique(as.integer(layerData$WAFER)))
  expect_equal(expected = ncol(fileExample), object = ncol(layerData))

})


test_that(desc = "getUniqueColumns works", code = {

  ids = getUniqueColumns(dataFrame = fileExample, columns = c("LOT", "WAFER"))
  expect_equal(expected = 2, object = ncol(ids))
  expect_equal(expected = 4, object = nrow(ids))
  expect_equal(expected = ids, object = unique(ids))

})


test_that(desc = "getKDEData works", code = {

  KDEFrame = getKDEData(dataFrame = fileExample, waferIdentificativeColumns = "LOT")
  expect_equal(expected = 8, object = nrow(KDEFrame))
  expect_equal(expected = 3, object = ncol(KDEFrame))
  expect_equal(expected = "LOTTO1", object = as.character(KDEFrame$ID[1]))

  KDEFrame = getKDEData(dataFrame = fileExample)
  expect_equal(expected = 12, object = nrow(KDEFrame))
  expect_equal(expected = 3, object = ncol(KDEFrame))
  expect_equal(expected = "LOTTO1/1/1", object = as.character(KDEFrame$ID[1]))
  expect_equal(expected = 6, object = KDEFrame$FAULT_X[1])
  expect_equal(expected = 11, object = KDEFrame$FAULT_Y[1])
  expect_equal(expected = 32, object = KDEFrame$FAULT_X[2])
  expect_equal(expected = 62, object = KDEFrame$FAULT_Y[2])

})
