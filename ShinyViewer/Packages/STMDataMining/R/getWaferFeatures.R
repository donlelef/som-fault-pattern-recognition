#' This function returns a list of all the dimensional features of the wafer in the data.frame passed.
#'
#' @title Get dimensional features from data.frame
#' @export
#' @param dataFrame a data.frame with the expected columns.
#' @param  dieWidthColumn the name of the column in dataFrame containing die widths
#' @param  dieHeightColumn the name of the column in dataFrame containing die heights
#' @param  waferSizeColumn the name of the column in dataFrame containing wafer diameters
#' @return a list of all the dimensional features of the wafer in the data.frame passed. In this list:
#' dieWidth: the width of a chip
#' dieHeight: the height of a chip
#' waferRay: the ray of the wafer


getWaferFeatures = function(dataFrame, dieWidthColumn = "DIE_WIDTH", dieHeightColumn = "DIE_HEIGHT", waferSizeColumn = "WAFER_SIZE"){

  # Extract data for the single wafer
  dieWidth = mean(dataFrame[[dieWidthColumn]], na.rm = TRUE)
  dieHeight = mean(dataFrame[[dieHeightColumn]], na.rm = TRUE)
  waferRay = mean(dataFrame[[waferSizeColumn]], na.rm = TRUE)/2

  resultList = list(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)
  return(resultList)
}
