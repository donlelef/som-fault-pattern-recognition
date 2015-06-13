#' Creates a circular grid from a square one by inserting the value outValue in
#' every pixel which is not whitin the circle of given ray. The width and the height
#' of the pixel may be specified. Every pixel is identified as a point placed in the 
#' center of its spatial extension. ie: if a chip's dimensions are 2x1, it is represent
#' as a point in (1;0.5).
#' @title Create a circular fault grid
#' @export
#' @param rectangularMap: a sqare matrix
#' @param alignmentX: the x coordinate of the center of the (0,0) chip
#' @param alignmentY: the y coordinate of the center of the (0,0) chip
#' @param dieWidth: the width of a chip
#' @param dieHeight: the height of a chip
#' @param waferRay: the ray of the wafer
#' @param outValue: the value assignad to the element of of the circle. Default is NA
#' @return the modified map

bindCircularMap = function(rectangularMap, dieWidth, dieHeight, alignmentX = 0, alignmentY = 0, waferRay, outValue = NA) {
  
  for (i in 1:nrow(rectangularMap)){
    for (j in 1:ncol(rectangularMap)){
      if ((-dieWidth/2 + i*dieWidth + alignmentX - waferRay)^2 + (-dieHeight/2 + alignmentY + j*dieHeight - waferRay)^2 >= waferRay^2)
        rectangularMap[i,j] = outValue;
    }
  }
  
  return(rectangularMap) 
}