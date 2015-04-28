#' Plot a bidimensional scatter plot using the given parameter. 
#' The title and other arguments may also be passed to te underlying plot function.
#' @title Plot a bidimensional scatter plot
#' @import plot3D
#' @param title: the title of the plot
#' @param x: values on the x axis.
#' @param y: values on the y axis.
#' @param ...: other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot3D

scatterPlot = function(title = "", x, y, ...){ 
  
  # import
  library(plot3D)
  
  scatter2D(x = x, y = y, pch = 4, 
            xlim = c(min(x),max(x)), ylim = c(min(y), max(y)),
            main = title, ...
  )
}
