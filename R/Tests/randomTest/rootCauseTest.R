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

# Divide good and bad wafer according to threshold
threshold = 10
selectedWafer = classificationResult[classificationResult$cluster == 1, ]
badWafers = selectedWafer[selectedWafer$faults > threshold, ]
goodWafers = selectedWafer[selectedWafer$faults < threshold, ]

# Consider only one operation
selectedOperation = 1
selectedStep = productionSteps[productionSteps$operations == selectedOperation, ]
badWafersOfOperation = badWafers[badWafers$lots %in% selectedStep$lots]
goodWafersOfOperation = goodWafers[goodWafers$lots %in% selectedStep$lots]
equipmentsOfOperation = productionSteps[productionSteps$operations == selectedOperation, ]$equipments

# Compute contingency table
for(selectedEquipment in equipmentsOfOperation){
  badEquipment = nrow(badWafersOfOperation[badWafersOfOperation$lot %in% selectedStep[selectedStep$equipments == selectedEquipment, ]$lots, ])
  goodEquipment = nrow(goodWafersOfOperation[goodWafersOfOperation$lot %in% selectedStep[selectedStep$equipments == selectedEquipment, ]$lots, ])
  badOther = nrow(badWafersOfOperation[badWafersOfOperation$lot %in% selectedStep[selectedStep$equipments != selectedEquipment, ]$lots, ])
  goodOther = nrow(goodWafersOfOperation[goodWafersOfOperation$lot %in% selectedStep[selectedStep$equipments != selectedEquipment, ]$lots, ])
  
  if(badEquipment/goodEquipment > badOther/goodOther){
    contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)
    print(chisq.test(contingencyMatrix, correct = FALSE))
  }
}


