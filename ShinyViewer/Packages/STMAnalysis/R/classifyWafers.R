#' Classify one or more wafers according to a trained kohonen object.
#'
#' @title Classify one or more wafers.
#' @export
#' @param trainedKohonenObject a \code{kohonen} object trained with wafers.
#' @param distributions a matrix where any row represent the smoothed fault probability function
#' on a wafer.
#' @import kohonen
#' @return vector where every element is the cluster of a wafer.

classifyWafers = function (distributions, trainedKohonenObject) {

  classifications = vector(mode = "numeric", length = nrow(distributions))

  for(i in 1:nrow(distributions)){

    thisWaferDistribution = matrix(distributions[i, ], nrow = 1)
    thisWaferClassification = map(x = trainedKohonenObject, newdata = thisWaferDistribution)$unit.classif
    classifications[i] = thisWaferClassification

  }

  return(classifications)
}
