#' This function selects the data of a single wafer by its lot and waferId.
#'
#' @title Select the data of a single wafer by its lot and waferId.
#' @export
#' @param lot the lot of the wafer
#' @param wafer the ID of the wafer
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with the same layout as dataFrame.


getWaferData = function(dataFrame, lot, wafer){

  waferData = dataFrame[dataFrame$LOT == lot & dataFrame$WAFER == wafer, ]
  return(waferData)
}
