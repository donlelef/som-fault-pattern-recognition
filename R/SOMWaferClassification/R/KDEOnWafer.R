#' Load data from a data frame. It it assumed that it has three column: the one
#' named wafer contains the name of the wafer, the one named faultX contains the 
#' x coord of the fault, while the faultY column contains the y coord of the 
#' faulty chip
#' 
#' @title Load a data.frame of faults into a matrix
#' @export
#' @param dayaFrame: a data.frame containin all the faults. The format should be the
#' one specified in the description
#' @param dieWidth: the width of the single die
#' @param dieHeigth: the heigth of the single die
#' @param waferRay: the ray of the wafer
#' @param plotDistributions: a boolean flag. If TRUE, all the computed distributions are
#' plotted. 
#' @import KDEFaultPattern ks
#' @return a matrix where any row represent the smoothed fault probability function
#' on a wafer.

KDEOnWafer = function (dataFrame, dieWidth, dieHeight, waferRay, plotDistributions = TRUE) {
  
  #Import
  library(ks)
  library(KDEFaultPattern)
  
  # Initializations
  waferNames = unique(dataFrame$wafer)
  grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay)
  distributions = matrix(data = 0, nrow = length(waferNames), ncol = length(grid$x)*length(grid$y))
  row = 1
  
  # Perform KDE
  for(wafer in waferNames){
    
    thisWaferDefects = dataFrame[dataFrame$wafer == wafer, ]
    faultPositions = as.matrix.data.frame(thisWaferDefects[ , 2:3])
    estimation = kde(x = faultPositions, 
                     gridsize = c(length(grid$x), length(grid$y)), 
                     xmin = c(min(grid$x), min(grid$y)), 
                     xmax = c(max(grid$x), max(grid$y)))
    
    # Plot every single wafer
    if(plotDistributions){
      extimatedFunction = bindCircularMap(rectangularMap = estimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, outValue = NA)
      title = paste("Extimated density function - ", wafer)
      matrixPlot(title = title, matrix = extimatedFunction, colorMap = rainbow(20))
      
    }

    # Preparing distributions for SOM
    extimatedFunction = bindCircularMap(rectangularMap = estimation$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, outValue = 0)
    distributions[row, ] = as.vector(extimatedFunction)
    row = row + 1
    
  }
  
  return(distributions)
}