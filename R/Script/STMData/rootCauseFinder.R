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

for(operation in unique(productionSteps$operations)){
  for(equipment in unique(productionSteps$equipments)){
    print(paste(operation, equipment, sep = " "))
    print(chiSquareTest(productionSteps = productionSteps, waferClustering = classificationResult, operation = operation, cluster = 1, equipment = equipment, threshold = 10))
  }
}
