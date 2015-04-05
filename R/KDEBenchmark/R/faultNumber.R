#' Computes the total number of faults in a wafer.
#' 
#' @title find total amount of faults
#' @param faultMap: a matrix containing the simulated chips
#' @param faultValue: the value assigned to a faulty chip. It cannot be NA nor NAN
#' @return the total amount of faulty chips.


faultNumber = function (faultMap, faultValue) {
  faultNumber = length(faultMap[(faultMap == faultValue)]) - length(faultMap[is.na(faultMap)])
  return(faultNumber)
}