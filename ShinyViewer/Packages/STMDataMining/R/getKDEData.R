#' This function return a data.frame suitable for KDE algorithm.
#' It contains the identifier of each wafer and the (x,y) coordinates of each fault.
#'
#' @title Return a data.frame suitable for KDE algorithm.
#' @export
#' @param dataFrame a data.frame with the supposed format.
#' @param waferIdentificativeColumns the columns that are to be used as identifiers of a wafer (ie. two wafers
#' are different if at least one of the value in these columns is different)
#' @param dieIdentificativeColumns the columns that are to be used as identifiers of a die (ie. two dies
#' are different if at least one of the value in these columns is different)
#' @param xFaultColumn the column contiaining the x coordinate of the fault.
#' @param yFaultColumn the column contiaining the y coordinate of the fault.
#' @param ... additional parameters to be passed to buildID()
#' @return a data.frame with three columns: ID,
#' FAULT_X (the x coordinate of the fault) and FAULT_Y (the y coordinate of the fault).
#' @import stats


getKDEData = function(dataFrame, waferIdentificativeColumns = c("LOT", "WAFER", "LAYER"), dieIdentificativeColumns = c("DIE_X", "DIE_Y"), xFaultColumn = "DIST_X", yFaultColumn = "DIST_Y", ...){

  faultIdsFrame = dataFrame[, c(waferIdentificativeColumns, dieIdentificativeColumns), drop = FALSE]
  faultIds = buildID(idsDataFrame = faultIdsFrame)
  faultIds = factor(faultIds, levels = unique(faultIds))
  ID = buildID(idsDataFrame = unique(faultIdsFrame)[, waferIdentificativeColumns, drop = FALSE], ...)
  
  aggregatedData = aggregate.data.frame(x = dataFrame[, c(xFaultColumn, yFaultColumn)], by = list(faultIds), FUN = mean)
  KDEData = data.frame(ID = ID, FAULT_X = aggregatedData[,2], FAULT_Y = aggregatedData[,3])
  return(KDEData)
}
