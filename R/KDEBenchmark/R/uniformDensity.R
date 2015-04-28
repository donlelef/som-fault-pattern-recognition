#' Computes a uniform probability density and return a list containing
#' the grid used to compute the probability and a matrix with the values of the 
#' probability function.
#' 
#' @title Return a uniform density
#' @import plot3D
#' @export
#' @param ray: half the number of column of the square matrix returned
#' @return a list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.

uniformDensity = function (ray) {
  
  # import
  library(plot3D) # required for mesh()
  
  x2 = x1 = seq(from = 0, to = 2*ray, length.out = 2*ray)
  
  pdf = (1/(pi*ray^2))*matrix(data = 1, nrow = 2*ray, ncol = 2*ray)
  grid = mesh(x1, x2)
  
  resultList = list(pdf = pdf, grid = grid)
  
  return(resultList)
}