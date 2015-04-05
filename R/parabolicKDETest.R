# This script cumputes a bidimensional gaussian distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# FALSE in the map means 'no fault' and TRUE means 'fault', whereas NA indicates 
# a point out of the circular wafer.

# Import required libraries
library(plot3D)   # Needed for mesh()
library(KernSmooth) # Needed for KDE
library(KDEBenchmark) 

# Initial parameters
ray = 30
mu = c(ray, ray)
maximumFaultProbability = 0.1

#Calcuate f(x) for a large number of possible values for x1 and x2
x = seq(from = -5, to = 5, length.out = 2*ray)
y = seq(from = -5, to = 5, length.out = 2*ray)
grid = mesh(x,y)
paraboloid = function(x,y){
  2/(pi*ray^4)*(x^2+y^2)
}
Z = outer(x,y,"paraboloid")

#3D plot with surf3D()
surf3D(x = grid$x, y = grid$y,  z = Z,  
       xlim = c(min(x),max(x)), ylim = c(min(y), max(y)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Normalized parabolic distribution")

# Fill the fault map
faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability, faultValue = 1, notFaultValue = 0)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = 0)

# Compute the fault number
faultNumber = length(faultMap[(faultMap == 1)]) - length(faultMap[is.na(faultMap)])

# Plot the fault map
par(pty = "s") # Force a square plot
image2D(
  x = 1:nrow(faultMap), y = 1:ncol(faultMap), z = faultMap, border = "black", 
  grid(nx=nrow(faultMap)), ny = ncol(faultMap),
  colkey = FALSE, NAcol = "white",  col = heat.colors(2)
) # Colkey = FALSE: no color key legend will be added

# KDE
faultIndex = which(faultMap == 1, arr.ind = TRUE)
estimation = bkde2D(faultIndex, bandwidth = 8)
grid = mesh(estimation$x1, estimation$x2)
surf3D(x = grid$x, y = grid$y, z = estimation$fhat,
       xlim = c(min(estimation$x1),max(estimation$x1)), ylim = c(min(estimation$x2), max(estimation$x2)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Extimated function")



