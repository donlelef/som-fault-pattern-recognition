#' Prepare a data.frame which with 5 colums: 
#' LOT: the processed lot
#' EQUIPMENT: the equipment which performed the operation
#' OPERATION: the stage of the manufactoring
#' QUANTITY: the number of processed wafers
#' TIME: when the operation happened
#'
#' @param dataFrame : a data frame which is supposed to contain at least
#' 5 columns, providing required data. Their name should be passed through
#' the following parameters.   
#' @param filterEquipment : a vector of strings, containg the names of the 
#' equipments which should be excluded from the history. If an equipment only
#' partially mathces with one of the elements, it is removed anyway. 
#' @param lotColumn : the name of the colum of dataFrame containing 
#' the name of the lot
#' @param equipmentColumn : the name of the colum of dataFrame containing 
#' the name of the equipment
#' @param opColumn : the name of the colum of dataFrame containing 
#' the name of the operation
#' @param quantityColumn : the name of the colum of dataFrame 
#' containing the quantity of processed wafers for each operation
#' @param timeColumn : : the name of the colum of dataFrame containing 
#' the time when the operation has been performed. It is supposed to be
#' in MM/DD/YYYY HH:MM format.
#'
#' @return A data.frame as stated in the description
#' @import lubridate
#' @export

getHistoryData = function(dataFrame, filterEquipment = c("DUMMY"), lotColumn = "LOT", equipmentColumn = "EQUIPMENT", opColumn = "OPER", quantityColumn = "QTY_OUT", timeColumn = "EVENT_TIME"){
  historyFrame = unique(dataFrame[, c(lotColumn, equipmentColumn, opColumn, quantityColumn, timeColumn), drop = FALSE] )
  colnames(historyFrame) = c("LOT", "EQUIPMENT", "OPERATION", "QUANTITY", "TIME")
  historyFrame = historyFrame[complete.cases(historyFrame), ]
  for (toFilter in filterEquipment) {
    historyFrame = historyFrame[!grepl(toFilter, historyFrame$EQUIPMENT, ignore.case = TRUE), ]
  }
  historyFrame$TIME = mdy_hm(historyFrame$TIME)
  return(historyFrame)
}