#' This function extracts the logical defect coordiantes form a properly formatted data.frame
#' 
#' @title Extract defect coordiantes
#' @export
#' @param waferData: the data.frame containing all the defect data 
#' @return a two-columns matrix where each row represent a pair of (x,y) coordinates.

getLogicalFaultCoords = function(waferData){
  
  STMDefectCoords = matrix(data = c(waferData$DIE_X, waferData$DIE_Y), ncol = 2, nrow = length(waferData$DIE_X))
  STMDefectCoords = unique(STMDefectCoords)
  
  return(STMDefectCoords)
  
}
