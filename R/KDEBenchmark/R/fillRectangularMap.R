#' Fills a grid whith two values according to the probabilityFuncion 
#' The higher is the value probabilityFunction takes in a cetrain point,the
#' higher is the probability that pixel is set to faultValue. The probabilityFuncion
#' is rescaled so that its maximum value is equal to maxFaultProbability.
#' @title Create a rectangular fault grid
#' @param probabilityFunction: a sqare matrix the probability distribution of faults
#' @param maxFaultProbability: a real positive number which represents the maximum
#' probability of a fault
#' @param faultVaue: the value assigned to a element which is faulty. Default is TRUE
#' @param notFaultVaue: the value assigned to a element which is faulty. Default is FALSE.
#' @return a matrix of the same dimensions of probabilityFunction.

fillRectangularMap = function(probabilityFunction, maxFaultProbability, faultValue = TRUE, notFaultValue = FALSE){
  faultMap = matrix(data = 0, nrow = nrow(probabilityFunction), ncol = ncol(probabilityFunction))
  for (i in 1:nrow(faultMap)){
    for (j in 1:ncol(faultMap)){
      if (runif(n = 1, min = 0, max = 1) < (probabilityFunction[i,j]/max(probabilityFunction))*maximumFaultProbability){
        faultMap[i,j] = faultValue
      }
      else{
        faultMap[i,j] = notFaultValue
      } 
    }
  }
  return(faultMap)
}