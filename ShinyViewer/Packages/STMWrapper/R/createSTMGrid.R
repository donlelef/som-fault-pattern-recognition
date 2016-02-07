#' This function creates the grid of all the chip centers in a wafer from
#' basic informations about the wafer.
#'
#' @title Generate the grid of chip centers
#' @export
#' @param dieWidth the width of a chip
#' @param dieHeight the height of a chip
#' @param waferRay the ray of the wafer
#' @param originalDieCenterX the x coordinate of the (0,0) chip. Default is 0.
#' @param originalDieCenterY the y coordinate of the (0,0) chip  Default is 0.
#' @return a list made of the x axis and the y axis.


createSTMGrid = function(waferRay, dieWidth, dieHeight, originalDieCenterX = 0, originalDieCenterY = 0){

  upperX = seq(from = originalDieCenterX, to = waferRay, by = dieWidth)
  lowerX = seq(from = originalDieCenterX, to = -waferRay, by = -dieWidth)
  upperY = seq(from = originalDieCenterY, to = waferRay, by = dieHeight)
  lowerY = seq(from = originalDieCenterY, to = -waferRay, by = -dieHeight)
  x = sort(unique(c(upperX, lowerX)))
  y = sort(unique(c(upperY, lowerY)))

  return(list(x=x, y=y))

}
