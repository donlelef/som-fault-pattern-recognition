#' Computes the sum of an arbitrary number of bivariate gaussian probability 
#' density and return a list containing the grid used to compute the 
#' probability and a matrix with the values of the probability function.
#' The values in the matrix are normalized to let the volume of the whole
#' system be equal to 1.
#' 
#' @title Return a the normalized sum of bivariate gaussian density
#' @export
#' @param axes: a list of 2 elements: the first is assumed to be the x axes
#' and the second the y axes where the function should be evaluated
#' @param parameterList: a list whose elements shuold be list, too. They are
#' assumed to contain the mean and the variace matrix of a single bivariate
#' normal function
#' @return a list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.

multiGaussianDensity = function (axes, parameterList) {
  
  x1 = axes[[1]]
  x2 = axes[[2]]
  
  pdf = matrix(data = 0, nrow = length(x1), ncol = length(x2))
  
  for(i in 1:length(parameterList)){
    list = gaussianDensity(axes = axes, mu = parameterList[[i]][[1]], sigma = parameterList[[i]][[2]])
    pdf = pdf + list$pdf/length(list)
  }
  
  grid = list$grid
  resultList = list(pdf = pdf, grid = grid)
  
  return(resultList)
}