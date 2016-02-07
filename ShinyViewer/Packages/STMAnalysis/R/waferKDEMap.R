#' This function perform KDE algorithm on a single wafer.
#'
#' @title KDE algorithm on a single wafer.
#' @export
#' @import ks
#' @param features a list containing the dimensional features of the wafer. Required fields are:
#' dieWidth: the width of a chip
#' dieHeight: the height of a chip
#' waferRay: the ray of the wafer
#' @param coords a matrix whise columns are the logical (ie. 1 die counts 1) coordinates of faults
#' @return a matrix whose elements represent the probability for each wafer to be faulty.


waferKDEMap = function(features, coords){

  grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  estimation = kde(x = coords, gridsize = c(length(grid$x), length(grid$y)), xmin = c(min(grid$x), min(grid$y)), xmax = c(max(grid$x), max(grid$y)))
  waferKDE = estimation$estimate

  return(waferKDE)
}
