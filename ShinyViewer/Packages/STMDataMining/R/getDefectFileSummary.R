#' This function return a data.frame containing some useful information 
#' about the data in a STM defect file.
#'
#' @title Return summary on a STM defect data frame.
#' @export
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with five columns:
#' LOTS: the number of different lots,
#' WAFERS: the number of different wafers,
#' LAYERS: the total number of different layers,
#' ROWS: the number of rows,
#' FAULTS: the total number of faulty chips.


getDefectFileSummary = function(dataFrame){

  lotsNumber = nrow(getUniqueColumns(dataFrame = dataFrame, columns = "LOT"))
  waferNumber = nrow(getUniqueColumns(dataFrame = dataFrame, columns = c("LOT", "WAFER")))
  layersNumber = nrow(getUniqueColumns(dataFrame = dataFrame, columns = c("LOT", "WAFER", "LAYER")))
  faultNumber = nrow(getUniqueColumns(dataFrame = dataFrame, columns = c("LOT", "WAFER", "LAYER", "DIE_X", "DIE_Y")))
  rowNumber = nrow(dataFrame)
  fileData = data.frame(LOTS = lotsNumber, WAFERS = waferNumber, LAYERS = layersNumber, FAULTS = faultNumber, ROWS = rowNumber)

  return(fileData)
}
