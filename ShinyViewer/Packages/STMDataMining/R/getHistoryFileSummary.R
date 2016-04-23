#' This function return a data.frame containing some useful 
#' information about the data in a STM lot processing history file.
#'
#' @title Return summary on a STM lot processing history data frame.
#' @export
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with five columns:
#' LOTS: the number of different lots,
#' OPERATIONS: the number of different operations,
#' EQUIPMENTS: the total number of different equipments,
#' START_DATE: the older date of an operation,
#' END_DATE: the most recent date of an operation
#' ROWS: the number of rows,


getHistoryFileSummary = function(dataFrame){
  
  lots = length(unique(dataFrame$LOT))
  equipments = length(unique(dataFrame$EQUIPMENT))
  operations = length(unique(dataFrame$OPER))
  startDate = min(mdy_hm(dataFrame$EVENT_TIME))
  endDate = max(mdy_hm(dataFrame$EVENT_TIME))
  rowNumber = nrow(dataFrame)
  fileData = data.frame(LOTS = lots, 
                          EQUIPMENTS = equipments, 
                          OPERATIONS = operations, 
                          START_DATE = startDate,
                          END_DATE = endDate,
                          ROWS = rowNumber)
  return(fileData)
}
