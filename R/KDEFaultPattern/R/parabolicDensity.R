#' Computes a parabolic probability density and return a list containing
#' the grid used to compute the probability and a matrix with the values of the 
#' probability function. The center of the paraboloid is the one of the wafer.
#' 
#' @title Return a parabolic density
#' @import plot3D
#' @export
#' @param axes: a list of 2 elements: the first is assumed to be the x axes
#' and the second the y axes where the function should be evaluated
#' @param coefficient: the coefficient a in the formula z = a(x^2 + y^2)
#' @param ray: the ray of the circular wafer
#' @return list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.


parabolicDensity = function (coefficient, ray, axes) {
  
  # import
  library(plot3D) # required for mesh()
  
  vectorParaboloid = function(x){
    if((x[1]-ray)^2 + (x[2]-ray)^2 < ray^2) {
      coefficient*((x[1]-ray)^2 + (x[2]-ray)^2)
    } else {
      0
    }
  }
  
  x1 = axes[[1]]
  x2 = axes[[2]]
  normalizationCoefficient = getNormalizationCoefficient(f = vectorParaboloid, lowerLimit = c(min(x1), min(x2)), upperLimit = c(max(x1), max(x2)))
  
  grid = mesh(x1,x2)
  paraboloid = function(x,y){
    normalizationCoefficient*coefficient*((x-ray)^2+(y-ray)^2)
  }
  Z = outer(x1, x2, "paraboloid")
  resultList = list(pdf = Z, grid = grid)
  
  return(resultList)
}