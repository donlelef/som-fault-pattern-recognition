#' Computes a uniform probability density and return a list containing
#' the grid used to compute the probability and a matrix with the values of the 
#' probability function.
#' 
#' @title Return a uniform density
#' @import plot3D
#' @export
#' @param axes: a list of 2 elements: the first is assumed to be the x axes
#' and the second the y axes where the function should be evaluated
#' @return a list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.

uniformDensity = function (axes) {
  
  # import
  library(plot3D) # required for mesh()
  
  x1 = axes[[1]]
  x2 = axes[[2]]
  
  pdf = 1/(pi*(max(x1)-min(x1))*(max(x2)-min(x2)))*matrix(data = 1, nrow = length(x1), ncol = length(x2))
  grid = mesh(x1, x2)
  
  resultList = list(pdf = pdf, grid = grid)
  
  return(resultList)
}