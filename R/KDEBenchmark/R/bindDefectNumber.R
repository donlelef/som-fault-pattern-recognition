#' Accept a matrix which is assumed to represent the map of faulty and 
#' good chips on a wafer. The fuction returns the same matrix where only 
#' the specified number of element is set to the value which represent a
#' fauly chip. In other words, this function samples a subset of faulty 
#' chips and saves the other. This is the reason why it is impossible to 
#' specify a number of faulty chip greater then the one of the matrix 
#' passed.
#' 
#' @title Create a map with a specified number of faults
#' @export
#' @param matrix: the matrix filled with faulty and good chips
#' @param faultValue: the value which identifies a faulty chip
#' @param notFaultValue: the value which identifies a good (not faulty) chip
#' @param faultNumber: the number of fault in the matrix which is returned
#' @return a matrix where exactly faultNumber elements are set to faultValue.


bindDefectNumber = function(matrix, faultValue, notFaultValue, faultNumber){
  
  faultPositions = which(matrix == faultValue, arr.ind = TRUE)
  matrix[faultPositions] = notFaultValue
  randomNewIndex = sample.int(n = nrow(faultPositions), size = faultNumber, replace = FALSE)
  faultPositions = faultPositions[randomNewIndex, ]
  matrix[faultPositions] = faultValue
  return(matrix)
  
}