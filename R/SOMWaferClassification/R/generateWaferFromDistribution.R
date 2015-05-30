#' Generates a set of wafer filled with good and faulty chips, according to the
#' fault probability distributions passed as an argument. The number of defect per
#' wafer is random, but it is ensured it is bind in the range passed as argument.
#' 
#' @title Generate a set of faulty wafer
#' @export
#' @param distributionsList: a list of matrix. Every element of the matrix is assumed
#' to represent the probability the chip in that position is faulty.
#' @param waferPerDistribution: a vector containing the number of wafer that are to
#' be generated for every distribution. The length of this vector should match the one 
#' of the distributionsList passed as an argument
#' @param defectsRange: a vector containing the higher and the 
#' lower number of defects in a wafer
#' @param dieWidth: the width of a single chip
#' @param dieHeight: the height of a single chip
#' @import KDEFaultPattern
#' @return a data.frame with 3 colums: the name of the wafer, the X coord of a defect, 
#' the Y coord of a defect. The origin is the lower left corner of the square a wafer
#' is inscribed in.

generateWaferFormDistribution = function (distributionsList, waferPerDistribution, defectsRange, dieWidth, dieHeight) {
  
  # Import
  library(KDEFaultPattern)
  
  # Initialization
  waferIDCounter = 1
  faultY = faultX =vector(mode = "numeric")
  wafers = vector(mode = "character")
  
  for (j in 1:length(distributionsList)) {
    
    for (i in 1:waferPerDistribution[j]) {
      # Fill a simulated wafer with good and bad chips according to the just computed density.
      faultMap = bindDefectNumber(probabilityMatrix = distributionsList[[j]], 
                                  faultValue = 1, notFaultValue = 0, 
                                  faultNumber = floor(runif(n = 1, min = defectsRange[1], max = defectsRange[2]))
      )
      # Find fault positions on the map and save them
      thisFaultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)
      thisWaferID = paste("wafer", waferIDCounter, sep = " ")
      
      faultX = c(faultX, thisFaultPositions[ ,1])
      faultY = c(faultY, thisFaultPositions[ ,2])
      wafers = c(wafers, rep(x = thisWaferID, times = nrow(thisFaultPositions)))
      waferIDCounter = waferIDCounter + 1
    }
  }
  
  # Saving the data
  data = data.frame(wafer = wafers, faultX = faultX, faultY = faultY)  
  
  return(data)
}