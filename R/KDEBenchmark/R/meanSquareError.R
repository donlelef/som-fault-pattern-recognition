#' Computes average square error between two matrix.
#' 
#' @title Find average square error.
#' @export
#' @param matrix1: the first matrix.
#' @param matrix1: the second matrix.
#' @param na.rm: if set to TRUE (as default), missing values are ignored
#' @return the average square error.


meanSquareError = function (matrix1, matrix2, na.rm = TRUE) {
  squareDifference = (matrix1 - matrix2)^2
  notNA = length(which(!is.na(squareDifference)))
  error = sum((matrix1 - matrix2)^2, na.rm = TRUE)/notNA
  return(error)
}