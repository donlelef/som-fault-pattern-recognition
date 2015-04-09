#' Computes a bivariate parabolic probability density and return a list containing
#' the grid used to compute the probability and a matrix with the values of the 
#' probability function. The center of the paraboloid is the one of the wafer.
#' 
#' @title Return a bivariate parabolic density
#' @import cubature
#' @param coefficient: the coefficient a in the formula z = a(x^2 + y^2)
#' @param ray: the ray of the circular wafer
#' @return list with 2 elemets: the values of the probability function "pdf" and the 
#' grid "grid" where it has been computed.


parabolicDensity = function (coefficient, ray) {
  
  vectorParaboloid = function(x){
    if((x[1]-ray)^2 + (x[2]-ray)^2 < ray^2) {
      coefficient*((x[1]-ray)^2 + (x[2]-ray)^2)
    } else {
      0
    }
  }
  
  x2 = x1 = seq(from = 0, to = 2*ray, length.out = 2*ray)  
  normalizationCoefficient = getNormalizationCoefficient(f = vectorParaboloid, lowerLimit = c(min(x1), min(x2)), upperLimit = c(max(x1), max(x2)))
  
  grid = mesh(x1,x2)
  paraboloid = function(x,y){
    normalizationCoefficient*coefficient*((x-ray)^2+(y-ray)^2)
  }
  Z = outer(x1, x2, "paraboloid")
  resultList = list(pdf = Z, grid = grid)
  
  return(resultList)
}