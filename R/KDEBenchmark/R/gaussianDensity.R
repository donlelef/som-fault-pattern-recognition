#' Computes a bivariate gaussian probability density and return the result
#' in the form of a matrix.
#' 
#' @title Gaussian Density
#' @param x1: sampling points on the x axes
#' @param x2: sampling points on the x axes
#' @param mu: a vector containing the mean of the gaussian density
#' @param signma: a matrix containing the variance of the gaussian density
#' @return a matrix containing the values of the density.

gaussianDensity = function (x1, x2, mu, sigma) {
  grid = expand.grid(x1, x2) #Creates all possible combinations
  densityVector = dmvnorm(x = grid, mean = mu, sigma = sigma, log = FALSE)
  
  # Arrange values in the following form:
  #         x2
  #         -5   -4.9 -4.8
  #x1 -5    f(x) f(x) f(x)
  #   -4.9  f(x) f(x) f(x)
  #   -4.8  f(x) f(x) f(x)
  #   ...
  Z = matrix(data = densityVector, nrow = length(x1), ncol = length(x2), byrow = FALSE)
  
  return(Z)
}