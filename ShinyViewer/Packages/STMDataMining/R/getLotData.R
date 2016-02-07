#' This function selects the data of a single lot by its lot name.
#'
#' @title Select the data of a single lot by its lot name.
#' @export
#' @param lot the lot of the wafer
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with the same layout as dataFrame.


getLotData = function(dataFrame, lot){

  waferData = dataFrame[dataFrame$LOT == lot, ]
  return(waferData)
}
