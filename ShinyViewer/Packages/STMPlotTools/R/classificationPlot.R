#' Visualize where an object has been placed by kohonen's SOM.
#' @title Plot a bidimensional matrix
#' @import kohonen
#' @export
#' @param kohonenObject the kohonen class object to plot
#' @param matchingColor the colors to be used for the matching cluster. Default is blue.
#' @param nonMatchingColor the colors to be used for non matching clusters. Default is white.
#' @param cluster the cluster where the object has been put.
#' @param ... other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot.kohonen

classificationPlot = function(kohonenObject, cluster, matchingColor = "blue", nonMatchingColor = "white", ...){

  oldPar = par(no.readonly = TRUE)
  colors = rep(x = nonMatchingColor, times = nrow(kohonenObject$codes))
  colors[cluster] = matchingColor
  plot(x = kohonenObject, type = "mapping", classif = cluster, bgcol = colors, ...)
  par(oldPar)

}
