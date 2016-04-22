#' Checks if a contingency matrix is significant for a yield issue.
#' The test is passed if no columns sums to zero - that is both the
#' equipment under test and the others processed at least one wafer
#' and if the yield of the equipment under test is lower than the
#' average.  
#'
#' @title Yield issue test for contingency matrix.
#' @param matrix a contingency matrix as described in createContingencyMatrix() 
#'
#' @return true if the tests are passed, false otherwise

checkContingencyMatrix = function(matrix){
  eqWafer = sum(matrix[ ,1])
  otherWafer = sum(matrix[ ,2])
  eqYield = matrix[2,1] / eqWafer
  otherYield =  matrix[2,2] / otherWafer
  isValid = (eqWafer != 0) && (otherWafer != 0) && eqYield < otherYield
  return(isValid)
}