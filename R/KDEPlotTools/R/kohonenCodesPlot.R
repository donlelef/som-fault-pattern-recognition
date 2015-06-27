#' Plot a series of subplots corresponding to the different codebooks stored in the
#' kohonen object.
#' @title Plot a bidimensional matrix
#' @import kohonen KDEFaultPattern
#' @export
#' @param kohonenObject: the kohonen class object to plot
#' @param colorMap: the colors to be used in the plot. Must be a valid argument for "col".
#' @param dieWidth: the width of the single die
#' @param dieHeigth: the heigth of the single die
#' @param waferRay: the ray of the wafer
#' @param grid: a list where the x element represents the array of the x coordinates 
#' of the chips' centers on the x axes, while the y element represents the array of the y coordinates 
#' of the chips' centers on the x axes.
#' @param ...: other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot.kohonen

kohonenCodesPlot = function(kohonenObject, dieWidth, dieHeight, waferRay, colorMap = rainbow(20), grid, ...){ 
  
  # import
  library(kohonen)
  library(KDEFaultPattern)
  
  oldPar = par()
  square = floor(sqrt(nrow(kohonenObject$codes)))
  par(mfrow=c(square, square), mar = c(1,1,1,1))
  for(i in 1:nrow(kohonenObject$codes)){
    wafer = matrix(data = kohonenObject$codes[i, ], nrow = length(grid$x), ncol = length(grid$y))
    wafer = bindCircularMap(rectangularMap = wafer, waferRay = waferRay, dieWidth = dieWidth, dieHeight = dieHeight, outValue = NA)
    matrixPlot(matrix = wafer, colorMap = colorMap, ...)
  }
  par(oldPar)
}
