#' This function returns a data.frame with the clustering of the data passed as an argument.
#' Clustering is performed through a SOM network with 3x3 grid.
#' @title Perform clustering for wafer data
#' @param faultPositionDataFrame: a data frame with three columns: the wafer Id in the first column and the X-Y 
#' coordiantes of the faults in the other two columns
#' @param dieWidth: the width of the single die
#' @param dieHeigth: the heigth of the single die.
#' @param waferRay: the ray of the wafer.
#' @param splitID: logical flag. If set to TRUE, the wafer ID is assumed to contain three tokens, separed by "/".
#' The name is split and in the returned data.frame the single parts are provided.
#' @param seed: the seed to be set for the inizialization of the som maps. Default is random.
#' @return A data frame containing the name of the wafer, the number of faulty chips and the cluster.
#' @import kohonen KDEFaultPattern STMWrapper
#' @export

writeClassificationRecap = function(faultPositionDataFrame, waferRay, dieWidth, dieHeight, splitID = TRUE, seed = as.integer(runif(n = 1, min = 0, max = 100))){
  
  # Import
  library(kohonen)
  library(KDEFaultPattern)
  library(STMWrapper)
  
  # Load data
  waferData = unique.data.frame(x = faultPositionDataFrame)
  
  # Perfarm KDE
  set.seed(seed)
  grid = createSTMGrid(waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight)
  distributions = KDEOnWafer(dataFrame = waferData, grid = grid, 
                             dieWidth = dieWidth, dieHeight = dieHeight, waferRay = waferRay, 
                             plotDistributions = FALSE)
  
  # Plot the SOM code vector after 100 iteration
  waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 100) 
  
  # Map each wafer to the correct unit
  # and save the data in vectors 
  # TODO: move to another function
  lot = wafer = layer = ids =vector(mode = "character")
  faults = cluster = vector(mode = "numeric")
  i = 1
  for(waferID in unique(waferData$wafers)){
    thisWaferData = waferData[waferData$wafers == waferID, ]
    thisWaferDisribution = matrix(distributions[i, ], nrow = 1)
    thisWaferClassification = map(x = waferSom, newdata = thisWaferDisribution)$unit.classif
    if(splitID){
      splittedID = unlist(strsplit(x = waferID, split = "[/]"))
      lot = c(lot, splittedID[1])
      wafer = c(wafer, splittedID[2])
      layer = c(layer, splittedID[3])
    } else {
      ids = c(ids, waferID)
    }
    faults = c(faults, nrow(thisWaferData))
    cluster = c(cluster, thisWaferClassification)
    i = i + 1
  }
  
  # Save the data of each wafer in a file
  if(splitID){
    waferRecap = data.frame(lot = lot, wafer = wafer, layer = layer, faults = faults, cluster = cluster)
  } else {
    waferRecap = data.frame(waferID = ids, faults = faults, cluster = cluster)
  }
  
  return(waferRecap)
}