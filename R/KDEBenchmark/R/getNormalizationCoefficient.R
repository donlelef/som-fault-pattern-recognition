#' Find the normalization coefficient for a given function in a given rectangular
#' region. In other words, this function returns the coefficient which is able to
#' make a function a probability function on a given region by normalizing its volume
#' to 1.
#' @title Return the normalization coefficient
#' @import cubature
#' @param f: the function to evaluate
#' @param lowerLimit: a vector containing the lower limits of the region
#' @param upperLimits: a vector containing the lower limits of the region
#' @param ...: other arguments to pass to the lower functions
#' @return the normalization coefficient
#' @seealso adaptIntegrate


getNormalizationCoefficient = function(f, lowerLimit, upperLimit, ...){
  
  # Import
  library("cubature") # Needed for normalization
  
  volume = adaptIntegrate(f, lowerLimit, upperLimit, maxEval = 100000)$integral
  normalizationCoefficient = 1/volume 
}