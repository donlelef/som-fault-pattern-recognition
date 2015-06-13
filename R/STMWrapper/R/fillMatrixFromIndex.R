#' This function fills a matrix of simbolic values according to the index passed 
#' as a parameter.
#' 
#' @title Fill a matrix according to parameters
#' @export
#' @param logicalCoords: a two-column matrix containing a couple of logical coordinates in each line - 
#' these coordinates are assumed to be index of a matrix, so they have to be positive integers
#' @param matchingValue: the value to put in any element of the matrix matching one of
#' the coordinates in logicalCoords.
#' @param matchingValue: the value to put in any element of the matrix not matching one of
#' the coordinates in logicalCoords.
#' @param nrow, ncol: dimensions of the reurned matrix.
#' @return a matrix filled as requested.


fillMatrixFromIndex = function(logicalCoords, matchingValue, nonMatchingValue, nrow, ncol){
  
  waferMap = matrix(data = nonMatchingValue, nrow = nrow, ncol = ncol)
  for(i in 1:nrow(logicalCoords)){
    waferMap[logicalCoords[i,1], logicalCoords[i,2]] = matchingValue
  }
  
  return(waferMap)
  
}
