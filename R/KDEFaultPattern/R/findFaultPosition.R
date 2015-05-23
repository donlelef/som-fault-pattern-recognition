#' Find the positions of the faults in the wafer. A chip is assumed to be represented as a point placed in the 
#' middle of its spatial extension, ie: if a chip's dimensions are 2x1, it is represent
#' as a poin in (1;0.5). This funcion returns the x and the y axes of the wafer, one point 
#' is placed for every chip.
#' 
#' @title Find the position of the faults
#' @export
#' @param dieWidth: the width of a chip
#' @param dieHeight: the height of a chip
#' @param faultValue: the value that indicates a fault on the map
#' @return a list of 2 elemets, containing the x and the y axes.

findFaultPositions = function(faultMap, dieWidth, dieHeight, faultValue){
  
  faultIndex = which(faultMap == faultValue, arr.ind = TRUE)
  faultPositions = matrix(data = 0, nrow = nrow(faultIndex), ncol = ncol(faultIndex))
  faultPositions[,1] = -dieWidth/2 + faultIndex[,1]*dieWidth
  faultPositions[,2] = -dieHeight/2 + faultIndex[,2]*dieHeight
  
  return(faultPositions)
}
