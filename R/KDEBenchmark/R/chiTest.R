#' Computes average square error between an extimated matrix and the real
#' one according to the chi test formula: 
#' error = 1/N(trueMatrix - extimatedMatrix)^2/trueMatrix.
#' 
#' @title Find weighed average square error.
#' @export
#' @param trueMatrix: the real matrix.
#' @param extiamtedMatrix: the extimated matrix.
#' @param na.rm: if set to TRUE (as default), missing values are ignored
#' @return the weighed average square error.


chiTest = function (trueMatrix, extimatedMatrix, na.rm = TRUE) {
  weighedSquareError = (trueMatrix - extimatedMatrix)^2/trueMatrix
  notNA = length(which(!is.na(weighedSquareError)))
  error = sum(weighedSquareError, na.rm = na.rm)/notNA
  return(error)
}