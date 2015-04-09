#' Computes a bivariate gaussian probability density and return a list containing
#' the grid used to compute the probability and a matrix with the values of the 
#' probability function.
#' 
#' @title Return a bivariate gaussian density
#' @import mvtnorm
#' @param x1: sampling points on the x axes
#' @param x2: sampling points on the x axes
#' @param mu: a vector containing the mean of the gaussian density
#' @param sigma: a matrix containing the variance of the gaussian density
#' @return a list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.

gaussianDensity = function (x1, x2, mu, sigma) {
  # Import
  library("mvtnorm") # Needed for dmvnorm()
  
  grid = expand.grid(x1, x2) #Creates all possible combinations
  densityVector = dmvnorm(x = grid, mean = mu, sigma = sigma, log = FALSE)
  
  # Arrange values in the following form:
  #         x2
  #         -5   -4.9 -4.8
  #x1 -5    f(x) f(x) f(x)
  #   -4.9  f(x) f(x) f(x)
  #   -4.8  f(x) f(x) f(x)
  #   ...
  pdf = matrix(data = densityVector, nrow = length(x1), ncol = length(x2), byrow = FALSE)
  grid = mesh(x1, x2)
  resultList = list(pdf = pdf, grid = grid)
  
  return(resultList)
}