#' Load data from a data frame and performs KDE on all wafers. It it assumed that it has three column: the one
#' named ID contains the name of the wafer, the one named FAULT_X contains the
#' x coord of the fault, while the FAULT_Y column contains the y coord of the
#' faulty chip.
#'
#' @title Performs KDE on a suitable data.frame
#' @export
#' @param KDEdataFrame a data.frame containin all the faults. The format should be the
#' one specified in the description.
#' @param features a list containing the dimensional features of the wafer. Required fields are:
#' dieWidth: the width of a chip
#' dieHeight: the height of a chip
#' waferRay: the ray of the wafer
#' @import STMFaultPattern ks
#' @return a matrix where any row represent the smoothed fault probability function on a wafer.

distributions = function (KDEdataFrame, features) {

  # Initializations
  waferNames = unique(KDEdataFrame$ID)
  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  distributions = matrix(data = 0, nrow = length(waferNames), ncol = length(grid$x)*length(grid$y))
  row = 1

  # Perform KDE
  for(id in waferNames){

    thisWaferData = subset(x = KDEdataFrame, subset = KDEdataFrame$ID == id)
    waferKDE = waferKDEMap(features = features, coords = thisWaferData[, 2:3])
    waferKDE = bindCircularMap(rectangularMap = waferKDE, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay, outValue = 0)
    distributions[row, ] = as.vector(waferKDE)

    row = row + 1

  }

  return(distributions)
}
