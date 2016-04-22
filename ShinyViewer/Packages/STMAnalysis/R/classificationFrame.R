#' This function returns a data.frame with the clustering of the data passed as an argument.
#' @title Writes data.frame with clustering data.
#' @param trainedSomObject : a kohonen object trained with the wafer in the KDEFrame
#' @param KDEFrame : a data frame with three columns: the wafer Id in the first column and the X-Y
#' coordiantes of the faults in the other two columns
#' @param KDEIds : name of the columns which are to contain the token form the ID column od KDEFrame
#' @return A data frame containing the name of the wafer, the number of faulty chips and the cluster.
#' @export
#' @import STMDataMining

classificationFrame = function(trainedSomObject, KDEFrame, KDEIds = c("LOT", "WAFER", "LAYER")){

  ids = unique(as.character(KDEFrame$ID))
  faults = vector(mode = "numeric")
  for(id in ids){
    faults = c(faults, nrow(subset(x = KDEFrame, subset = KDEFrame$ID == id)))
  }
  idsFrame = splitID(ids = ids)
  res = cbind.data.frame(idsFrame, faults, trainedSomObject$unit.classif)
  colnames(res) = c(KDEIds[1:ncol(idsFrame)], "FAULTS", "CLUSTER")

  return(res)
}
