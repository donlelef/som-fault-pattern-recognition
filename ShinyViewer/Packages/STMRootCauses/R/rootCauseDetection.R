#' Perform chi square test through contingency matrix
#' on every equipment, for each operation.
#' @title Find the equipments with the lowest yields.
#' @param historyFrame a data.frame containing four columns:
#' EQUIPMENT: the equipment which performed an operation
#' OPERATION: an action performed on a lot
#' LOT: a lot of wafer
#' QUANTITY: the number of wafers in the lot
#' @param badWafers a data.frame containing at least two columns: LOT, 
#' that is the lot of the wafer, and QUANTITY, that is the number of wafer in that lot.
#' It is supposed to contain all the wafers to be considered bad.
#'
#' @return a data.frame which contains, for each OPERATION and EQUIPMENT,
#' the result of chiTestEquipment()


rootCauseDetection = function(historyFrame, badWafers) {
  res = data.frame()
  
  for (operation in unique(historyFrame$OPERATION)) {
    opHist = historyFrame[historyFrame$OPERATION == operation, ]
    for (equipment in unique(opHist$EQUIPMENT)) {
      thisEqWafers = opHist[opHist$EQUIPMENT == equipment, ]
      otherEqWafers = opHist[opHist$EQUIPMENT != equipment, ]
      contingencyMatrix = createContingencyMatrix(thisEqWafers = thisEqWafers, otherEqWafers = otherEqWafers, badWafers = badWafers)
      if(checkContingencyMatrix(contingencyMatrix)){
        testValue = chiTestEquipment(contingencyMatrix)
        res = rbind(res, data.frame(OPERATION = operation,
                                    EQUIPMENT = equipment, 
                                    testValue,
                                    stringsAsFactors = FALSE))  
      }
    }
  }
  res = res[order(res$VALUE, decreasing = TRUE), ]
}