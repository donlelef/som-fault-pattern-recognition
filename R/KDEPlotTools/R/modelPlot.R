#' Plot a bidimensional line plot using the given parameter. 
#' The title and other arguments may also be passed to te underlying plot function.
#' @title Plot a function as a line
#' @import graphics
#' @export
#' @param title: the title of the plot
#' @param x: values on the x axis.
#' @param y: values on the y axis.
#' @param xlim: a vector containing the lower and the upper limit on the x axis
#' @param ylim: a vector containing the lower and the upper limit on the y axis
#' @param ...: other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot3D

modelPlot = function(title = "", x, y, xlim, ylim, ...){ 
  
  # import
  library(graphics)
  
  plot(x = x, y = y, xlim = xlim, ylim = ylim,
       type = "l", xlab = "", ylab = "", axes = FALSE, ...
  )
  
}
