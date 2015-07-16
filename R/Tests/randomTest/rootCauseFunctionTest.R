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

# Define function
rootCause = function(productionSteps, waferClustering, operation, cluster, equipment, threshold = mean(waferClustering$faults)){
  
  contingencyMatrix = createContingencyMatrix(productionSteps, waferClustering, operation, cluster, equipment, threshold)
  print(contingencyMatrix)
  if(all(contingencyMatrix[ , 1] == 0)){
    return("No wafer for the selected equipment") 
  }
  if(all(contingencyMatrix[ , 2] == 0)){
    return("No wafer for other selected equipments") 
  }
  if(contingencyMatrix[1,1]/contingencyMatrix[2,1] > contingencyMatrix[2,1]/contingencyMatrix[2,2]){
    return(chisq.test(contingencyMatrix, correct = FALSE))
  }
  
  return("Yield above average") 
}

createContingencyMatrix = function(productionSteps, waferClustering, operation, cluster, equipment, threshold = mean(waferClustering$faults)){
  
  # Divide good and bad wafer according to threshold
  selectedWafer = classificationResult[classificationResult$cluster == cluster, ]
  badWafers = selectedWafer[selectedWafer$faults > threshold, ]
  goodWafers = selectedWafer[selectedWafer$faults < threshold, ]
  
  # Consider only one operation
  selectedStep = productionSteps[productionSteps$operations == operation, ]
  badWafersOfOperation = badWafers[badWafers$lots %in% selectedStep$lots]
  goodWafersOfOperation = goodWafers[goodWafers$lots %in% selectedStep$lots]
  equipmentsOfOperation = productionSteps[productionSteps$operations == operation, ]$equipments
  
  # Compute contingency table
  badEquipment = nrow(badWafersOfOperation[badWafersOfOperation$lot %in% selectedStep[selectedStep$equipments == equipment, ]$lots, ])
  goodEquipment = nrow(goodWafersOfOperation[goodWafersOfOperation$lot %in% selectedStep[selectedStep$equipments == equipment, ]$lots, ])
  badOther = nrow(badWafersOfOperation[badWafersOfOperation$lot %in% selectedStep[selectedStep$equipments != equipment, ]$lots, ])
  goodOther = nrow(goodWafersOfOperation[goodWafersOfOperation$lot %in% selectedStep[selectedStep$equipments != equipment, ]$lots, ])
  
  contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)
  return(contingencyMatrix)
}

for(operation in unique(productionSteps$operations)){
  for(equipment in unique(productionSteps$equipments)){
    print(paste(operation, equipment, sep = " "))
    print(rootCause(productionSteps = productionSteps, waferClustering = classificationResult, operation = operation, cluster = 1, equipment = equipment, threshold = 10))
  }
}
