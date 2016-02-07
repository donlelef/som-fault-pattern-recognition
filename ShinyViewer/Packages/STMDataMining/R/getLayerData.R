#' This function selects the data of a single wafer by its lot, waferId and layer.
#'
#' @title Selects the data of a single wafer by its lot, waferId and layer.
#' @export
#' @param lot the lot of the wafer
#' @param wafer the ID of the wafer
#' @param layer the layer of the wafer
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with the same layout as dataFrame.


getLayerData = function(dataFrame, lot, wafer, layer){

  waferData = dataFrame[dataFrame$LOT == lot & dataFrame$WAFER == wafer & dataFrame$LAYER == layer, ]
  return(waferData)
}
