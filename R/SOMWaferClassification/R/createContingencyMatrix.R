#' This function creates a contingency matrix from two dataset, containing the production steps and the result of wafer clustering,
#' respectively.
#' The contingency matrix is a 2x2 numeric matrix M. 
#' M11 = bad wafer processed by the selected equipment
#' M12 = bad wafer processed by other equipments
#' M21 = good wafer processed by the selected equipment
#' M22 = bad wafer processed by other equipments
#' @title Contingency matrix form datasets.
#' @param productionSteps: a data.frame where the first column is assumed to contain the lot, the second one the equipment and the 
#' third the operations during the production chain
#' @param waferClustering: a data.frame where the first column is assumed to contain the lot, the second one the wafer ID and the 
#' third the number of faults and the last the cluster as returned by SOM algorithm.
#' @param operation: the operation (ie. the production step to be tested)
#' @param cluster: the cluster to investigate
#' @param equipment: the equipment whose yield is to be tested
#' @param threshold: the maximum nuber of fault a good wafer could have. Default is the average of the waferClustering data.frame.
#' @return the 2x2 contingency matrix

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