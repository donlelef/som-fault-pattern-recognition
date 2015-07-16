# Thest for chi square test to find the root cause of a fault pattern

library(SOMWaferClassification)

# Generate data
lots = c("lot1", "lot2", "lot3", "lot1")
equipments = c("e1", "e2", "e3", "e4")
operation = c(rep(1, 3), 2)
productionSteps = data.frame(lots = lots, equipments = equipments, operations = operation)

lots = c(rep("lot1", 3), rep("lot2", 2) , rep("lot3", 2))
wafers = c(1:3, 1:2, 1:2)
faults = c(rep(100, 4), rep(0,3))
cluster = rep(1, 7)
classificationResult = data.frame(lots = lots, wafers = wafers, faults = faults, cluster = cluster)

operations = equipments = vector(mode = "character")
testedValues = vector(mode = "numeric")
for(thisOperation in unique(productionSteps$operations)){
  for(thisEquipment in unique(productionSteps$equipments)){
    testResult = chiSquareTest(productionSteps = productionSteps, waferClustering = classificationResult, operation = thisOperation, cluster = 1, equipment = thisEquipment, threshold = 10)
    if(!is.character(testResult)){
      operations = c(operations, thisOperation)
      equipments = c(equipments, thisEquipment)
      testedValues = c(testResult$statistic, testedValues)
    }
  }
}
testData = data.frame(operation = operations, equipment = equipments, value = testedValues)
testData = testData[order(testData[ ,3], decreasing = TRUE), ]

labels = paste("Op.", testData$operation, "Eq.", testData$equipment, sep = " ")
barplot(testData$value, main = "Equipment faulty probability", ylab = "X-Value", 
        names.arg = labels, col = "blue",
        ylim = c(0, max(testData$value) + 0.1 * max(testData$value)))
