#' Plot a bidimensional matrix using the given colorMap. The title and other arguments
#' may also be passed to te underlying plot function
#' @title Plot a bidimensional matrix
#' @import plot3D
#' @export
#' @param title: the tithe of the plot
#' @param matrix: the matrix to be plotted
#' @param colorMap: the colors to be used in the plot. Must be a valid argument for "col".
#' @param ...: other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot3D

plotMatrix = function(title = "", matrix, colorMap, ...){ 
 
  # import
  library(plot3D)
  
  image2D(
    x = 1:nrow(matrix), y = 1:ncol(matrix), z = matrix, border = "black",
    zlim = c(0, max(matrix, na.rm = TRUE)),
    grid(nx=nrow(matrix), ny = ncol(matrix)),
    colkey = FALSE, NAcol = "black",  col = colorMap,
    main = title, xlab = "", ylab = "", ...    
  )
}
