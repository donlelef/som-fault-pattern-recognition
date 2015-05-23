#' Accept a matrix which is assumed to represent the probability a single chip
#' on the wafer is to be faulty. The fuction returns the a matrix of the same
#' dimension of the passed one matrix where only the specified number of element 
#' is set to the value which represent a fauly chip. 
#' In other words, this function samples a subset of the chips to be faulty
#' and saves the other. 
#' 
#' @title Create a map with a specified number of faults
#' @export
#' @param probabilityMatrix: the matrix which represent the probability of every chip to be faulty
#' @param faultValue: the value which identifies a faulty chip
#' @param notFaultValue: the value which identifies a good (not faulty) chip
#' @param faultNumber: the number of fault in the matrix which is returned
#' @return a matrix where exactly faultNumber elements are set to faultValue.


bindDefectNumber = function(probabilityMatrix, faultValue, notFaultValue, faultNumber){
  
  faultMap = matrix(data = notFaultValue, nrow = nrow(probabilityMatrix), ncol = ncol(probabilityMatrix))
  faultIndex = sample(x = 1:length(faultMap), size = faultNumber, replace = FALSE, prob = as.vector(probabilityMatrix))
  faultMap[faultIndex] = faultValue
  return(faultMap)
  
}