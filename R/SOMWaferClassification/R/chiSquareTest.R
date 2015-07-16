#' This function creates a contingency matrix from two dataset, containing the production steps and the result of wafer clustering,
#' respectively.
#' The test deals with the probability of a particular equipment to show a lower yield than the average one. The higher is
#' the value of the test, the most likely is the equipment to be faulty.
#' @title Contingency matrix form datasets.
#' @param productionSteps: a data.frame where the first column is assumed to contain the lot, the second one the equipment and the 
#' third the operations during the production chain
#' @param waferClustering: a data.frame where the first column is assumed to contain the lot, the second one the wafer ID and the 
#' third the number of faults and the last the cluster as returned by SOM algorithm.
#' @param operation: the operation (ie. the production step to be tested)
#' @param cluster: the cluster to investigate
#' @param equipment: the equipment whose yield is to be tested
#' @param threshold: the maximum nuber of fault a good wafer could have. Default is the average of the waferClustering data.frame.
#' @return either an object as returned by chisq.test or a string explaining why the test could not have been performed
#' @import stats
#' @export
 

chiSquareTest = function(productionSteps, waferClustering, operation, cluster, equipment, threshold = mean(waferClustering$faults)){
  
  contingencyMatrix = createContingencyMatrix(productionSteps, waferClustering, operation, cluster, equipment, threshold)
  print(contingencyMatrix)
  if(all(contingencyMatrix[ , 1] == 0)){
    return("No wafer for the selected equipment") 
  }
  if(all(contingencyMatrix[ , 2] == 0)){
    return("No wafer for other selected equipments") 
  }
  if(contingencyMatrix[1,1]/contingencyMatrix[2,1] > contingencyMatrix[2,1]/contingencyMatrix[2,2]){
    return(chisq.test(contingencyMatrix, correct = FALSE))
  }
  
  return("Yield above average") 
}
