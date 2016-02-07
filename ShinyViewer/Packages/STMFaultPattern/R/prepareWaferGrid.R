#' Prepare a grid which is assumed to represent the wafer: the width and the height
#' of a single chip may be specified. A chip is represent as a point placed in the
#' middle of its spatial extension, ie: if a chip's dimensions are 2x1, it is represent
#' as a poin in (1;0.5). This funcion returns the x and the y axes of the wafer, one point
#' is placed for every chip.
#'
#' @title Prepare the axes for the wafer
#' @export
#' @param dieWidth the width of a chip
#' @param dieHeight the height of a chip
#' @param waferRay the ray of the wafer
#' @return a list of 2 elemets, containing the x and the y axes.

prepareWaferGrid = function(dieWidth, dieHeight, waferRay){

  x1 = seq(from = dieWidth/2, to = 2*waferRay - dieWidth/2, by = dieWidth)
  x2 = seq(from = dieHeight/2, to = 2*waferRay - dieHeight/2, by = dieHeight)

  grid = list(x = x1, y = x2)

  return(grid)

}
