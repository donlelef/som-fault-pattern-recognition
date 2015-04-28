#' Computes the sum of an arbitrary number of bivariate gaussian probability 
#' density and return a list containing the grid used to compute the 
#' probability and a matrix with the values of the probability function.
#' The values in the matrix are normalized to let the volume of the whole
#' system be equal to 1.
#' 
#' @title Return a the normalized sum of bivariate gaussian density
#' @export
#' @param ray: half the number of column of the square matrix returned
#' @param parameterList: a list whose elements shuold be list, too. They are
#' assumed to contain the mean and the variace matrix of a single bivariate
#' normal function
#' @return a list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.

multiGaussianDensity = function (ray, parameterList) {
  
  pdf = matrix(data = 0, nrow = 2*ray, ncol = 2*ray)
  
  for(i in 1:length(parameterList)){
    list = gaussianDensity(ray = ray, mu = parameterList[[i]][[1]], sigma = parameterList[[i]][[2]])
    pdf = pdf + list$pdf/length(list)
  }
  
  grid = list$grid
  resultList = list(pdf = pdf, grid = grid)
  
  return(resultList)
}