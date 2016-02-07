#' This function fills a matrix of simbolic values according to the index passed
#' as a parameter.
#'
#' @title Fill a matrix according to parameters
#' @export
#' @param index a two-column matrix containing a couple of index in each line -
#' these are assumed to be index of a matrix, so they have to be positive integers.
#' @param matchingValue the value to put in any element of the matrix matching one of
#' the coordinates in index.
#' @param nonMatchingValue the value to put in any element of the matrix not matching one of
#' the coordinates in index.
#' @param nrow number of rows of the returned matrix.
#' @param ncol number of columns of the returned matrix.
#' @return a matrix filled as requested.


fillMatrixFromIndex = function(index, matchingValue, nonMatchingValue, nrow, ncol){

  waferMap = matrix(data = nonMatchingValue, nrow = nrow, ncol = ncol)
  for(i in 1:nrow(index)){
    waferMap[index[i,1], index[i,2]] = matchingValue
  }

  return(waferMap)

}
