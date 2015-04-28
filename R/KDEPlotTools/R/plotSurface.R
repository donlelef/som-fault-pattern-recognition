#' Plot a tridimensional surface using the given parameter. The title and other arguments
#' may also be passed to te underlying plot function.
#' @title Plot a tidimensional surface
#' @import plot3D
#' @param title: the tithe of the plot
#' @param x: values on the x axis.
#' @param y: values on the y axis.
#' @param z: values on the z axis - ie the value of the function at the coordinates (x,y).
#' @param ...: other arguments to pass to the lower functions
#' @return nothing: just perform the plot
#' @seealso plot3D

plotSurface = function(title = "", x, y, z, ...){ 
  
  # import
  library(plot3D)
  
  surf3D(x = x, y = y, z = z,  
         xlim = c(min(x),max(x)), ylim = c(min(y), max(y)),
         lighting = TRUE, phi = 30, theta = 45, bty = "b2", 
         main = title, ...
  )
}
