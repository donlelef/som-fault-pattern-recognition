#' Return the best bandwidth from the vector of the fitted bandwidth and 
#' the number of faults on the wafer. Another vector should be passed to the function:
#' it is assumed to contain the amounts of faults every fitted bandwidth was
#' computed for
#' 
#' @title Find the best bandwidth.
#' @export
#' @param fittedBandwidth: a vector containing the fitted bandwidths.
#' @param fitttedFaults: a vector containing the numbers of faults bandwidths was computed for.
#' @param faults: the amount of faults on the wafer.
#' @return the best bandwidth.


bestBandwidth = function (fittedBandwidth, fittedFaults, faults) {
  index = which(fittedFaults == faults)
  if(faults <= min(fittedFaults)){
    index = 1
  } else if(faults >= max(fittedFaults)){
    index = length(fittedBandwidth)
  } 
  
  bestBandwidth = fittedBandwidth[index]
  
  return(bestBandwidth)
  
}