#' Perform a root cause analysis algorithm and finds the equipments whose
#' yield is under the average.
#' @title Root cause analysis
#' @param classificationFrame at least three columns:
#' LOT: the name of a wafer lot
#' FAULTS: the number of fault of a wafer
#' CLUSTER: the cluster of a wafer
#' Each row of the data frame is supposed to represent a single wafer
#' @param historyFrame a data.frame containing four columns:
#' EQUIPMENT: the equipment which performed an operation
#' OPERATION: an action performed on a lot
#' LOT: a lot of wafer
#' QUANTITY: the number of wafers in the lot
#' @param threshold the number of faults a wafer should have to be considered bad
#' @param clustrers the cluster to include in the root cause analysis
#'
#' @return a data.frame which contains, for each OPERATION and EQUIPMENT,
#' VALUE: the chi sqare value
#' BAD_EQUIPMENT: the number of bad wafers processed by the equipment the contingency matrix refers to
#' GOOD_EQUIPMENT: the number of bad wafers processed by the equipment the contingency matrix refers to
#' BAD_OTHER: the number of bad wafers processed by other equipments
#' GOOD_OTHER: the number of good wafers processed by other equipments
#' P_VALUE: the p value of the chi square statistic
#' @export

rootCauseAnalysis = function(classificationFrame, historyFrame, threshold, clustrers){
  
  clustrers = as.numeric(clustrers)
  
  classificationFrame = classificationFrame[classificationFrame$CLUSTER %in% clustrers, ]
  badWafers = classificationFrame[classificationFrame$FAULTS >= threshold, ]
  historyFrame = historyFrame[historyFrame$LOT %in% unique(classificationFrame$LOT), ]
  historyFrame = historyFrame[order(historyFrame$TIME), ]
  res = rootCauseDetection(historyFrame, badWafers)
  
  return(res)
}