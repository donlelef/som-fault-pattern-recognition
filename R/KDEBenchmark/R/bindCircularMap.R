#' Creates a circular grid from a square one by inserting the value outValue in
#' every pixel which is not whitin the circle of given ray.
#' @title Create a circular fault grid
#' @param rectangularMap: a sqare matrix
#' @param ray: the ray of the circular map
#' @param outValue: the value assignad to the element of of the circle. Default is NA
#' @return the modified map

bindCircularMap = function(rectangularMap, ray, outValue = NA) {
  
  for (i in 1:nrow(rectangularMap)){
    for (j in 1:ncol(rectangularMap)){
      if ((i-ray)^2 + (j-ray)^2 >= ray^2)
        rectangularMap[i,j] = outValue;
    }
  }
  
  return(rectangularMap) 
}