#' This function builds a fault map for a selected wafer.
#'
#' @title Build the fault map for a wafer.
#' @export
#' @import STMFaultPattern
#' @param features a list containing the dimensional features of the wafer. Required fields are:
#' dieWidth: the width of a chip
#' dieHeight: the height of a chip
#' waferRay: the ray of the wafer
#' @param index a matrix whise columns are the index (ie. 1 die counts 1) of faults.
#' These numbers are assumed to be index of a matrix, so they jhave to be positive integers.
#' @param faultValue the value to represent a faulty chip. Default is 1
#' @param nonFaultValue the value to represent a good chip. Default is 0
#' @param outValue the value to represent a chip outside the circular wafer. Default is NA
#' @return a matrix whose elements represent dies on the wafer. The value of each element is a conventional
#' symbol for its condition (good, bad, out of wafer).


waferFaultMap = function(features, index, faultValue = 1, nonFaultValue = 0, outValue = NA){

  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  waferMap = fillMatrixFromIndex(index = index, matchingValue = faultValue, nonMatchingValue = nonFaultValue, nrow = length(grid$x), ncol = length(grid$y))
  waferMap = bindCircularMap(rectangularMap = waferMap, dieWidth = features$dieWidth, dieHeight = features$dieHeight, waferRay = features$waferRay, outValue = NA)

  return(waferMap)
}
