#' Create a contingency matrix m where:
#' m[1, 1] = bad wafers processed by an equipment
#' m[1, 2] = good wafers processed by an equipment
#' m[2, 1] = bad wafers processed by other equipments
#' m[2, 2] = good wafers processed by other equipments
#'
#' @title Create contingency matrix from wafer data
#' @param thisEqWafers a data.frame containing at least two columns: LOT, 
#' that is the lot of the wafer, and QUANTITY, that is the number of wafer in that lot.
#' It is supposed to contain all the wafers processed by a single equipment.
#' @param otherEqWafers a data.frame containing at least two columns: LOT, 
#' that is the lot of the wafer, and QUANTITY, that is the number of wafer in that lot.
#' It is supposed to contain all the wafers processed by other equipment.
#' @param badWafers a data.frame containing at least two columns: LOT, 
#' that is the lot of the wafer, and QUANTITY, that is the number of wafer in that lot.
#' It is supposed to contain all the wafers to be considered bad.
#' @return the contingency matrix

createContingencyMatrix = function(thisEqWafers, otherEqWafers, badWafers) {
  
  badEquipment = nrow(badWafers[badWafers$LOT %in% thisEqWafers$LOT, ])
  goodEquipment = sum(unique.data.frame(thisEqWafers[, 1:4])$QUANTITY) - badEquipment
  badOther = nrow(badWafers[badWafers$LOT %in% otherEqWafers$LOT, ])
  goodOther = sum(unique.data.frame(otherEqWafers[, 1:4])$QUANTITY) - badOther
  
  contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)

  return(contingencyMatrix)
}