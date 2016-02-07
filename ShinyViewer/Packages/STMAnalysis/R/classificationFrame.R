#' This function returns a data.frame with the clustering of the data passed as an argument.
#' @title Writes data.frame with clustering data.
#' @param trainedSomObject a kohonen object trained with the wafer in the KDEFrame
#' @param KDEFrame a data frame with three columns: the wafer Id in the first column and the X-Y
#' coordiantes of the faults in the other two columns
#' @return A data frame containing the name of the wafer, the number of faulty chips and the cluster.
#' @export

classificationFrame = function(trainedSomObject, KDEFrame){

  ids = unique(KDEFrame$ID)
  faults = vector(mode = "numeric")
  for(id in ids){
    faults = c(faults, nrow(subset(x = KDEFrame, subset = KDEFrame$ID == id)))
  }
  classificationFrame = data.frame(ID = ids, FAULTS = faults, CLUSTER = trainedSomObject$unit.classif)

  return(classificationFrame)
}
