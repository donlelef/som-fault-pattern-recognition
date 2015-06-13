#' This function turns logical coordinates in the absolute position
#' of the center of a chip.
#' 
#' @title Turn locical to absolute coordinates
#' @export
#' @param dieWidth: the width of a chip
#' @param dieHeight: the height of a chip
#' @param logicalCoords: a matrix containing a couple of coordinates in each line
#' @param originalDieCenterX: the x coordinate of the (0,0) chip
#' @param originalDieCentery: the y coordinate of the (0,0) chip 
#' @return a matrix whose rows are (x,y) absolute coordinats. The origin is the center of the wafer.


getCoordsFromLogical = function(logicalCoords, dieWidth, dieHeigth, originalDieCenterX, originalDieCenterY){
  
  DieCenterDefectCoords = matrix(data = 0, ncol = 2, nrow = length(logicalCoords[, 1]))
  DieCenterDefectCoords[,1] = originalDieCenterX + logicalCoords[,1]*dieWidth 
  DieCenterDefectCoords[,2] = originalDieCenterY + logicalCoords[,2]*dieHeight
  
  return(DieCenterDefectCoords)
  
}
