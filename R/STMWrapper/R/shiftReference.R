#' This function turns logical coordinates in the absolute position
#' of the center of a chip.
#' 
#' @title Shift the orign of the reference to the left lower corner
#' @export
#' @param logicalCoords: a matrix containing a couple of logical coordinates in each line - 
#' ie: the fist chip is 0, the following 1, the previous -1, ...
#' @param grid: the positions of all the centers of the chips in the wafer
#' @param originalDieCenterX: the x coordinate of the (0,0) chip
#' @param originalDieCentery: the y coordinate of the (0,0) chip 
#' @return a matrix whose rows are logical coordinats. The origin is the left corner of the wafer.


shiftReference = function(logicalCoords, originalDieCenterX, originalDieCenterY, grid){
  
  shiftedCoords = matrix(data = 0, nrow = nrow(logicalCoords), ncol = ncol(logicalCoords))
  shiftedCoords[,1] = logicalCoords[,1] + length(grid$x[grid$x<originalDieCenterX]) + 1 
  shiftedCoords[,2] = logicalCoords[,2] + length(grid$y[grid$y<originalDieCenterY]) + 1
  
  return(shiftedCoords)
  
}
