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
maximumFaultProbability = 0.1
coefficient = 1

#Calcuate f(x) for a large number of possible values for x1 and x2
list = parabolicDensity(coefficient = coefficient, ray = ray)
Z = list$pdf
grid = list$grid

# Fill the fault map
faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability, faultValue = 1, notFaultValue = 0)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = NA)

# Compute the fault number
faultNumber = faultNumber(faultMap = faultMap, faultValue = 1)

# Perform the KDE
faultIndex = which(faultMap == 1, arr.ind = TRUE)
estimation = bkde2D(x = faultIndex, bandwidth = 8, range.x = list(c(0,2*ray), c(0, 2*ray)), gridsize = c(2*ray, 2*ray))

# 3D plot of the fault probability density with surf3D()
Z = bindCircularMap(rectangularMap = Z, ray = ray, outValue = NA)
surf3D(x = grid$x, y = grid$y,  z = Z,  
       xlim = c(0,2*ray), ylim = c(0, 2*ray),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Normalized parabolic distribution"
)

# Plot the fault map
par(pty = "s") # Force a square plot
plotMatrix(title = "Fault map", matrix = faultMap, colorMap = heat.colors(2), 
           sub = bquote("Number of faults = "~.(faultNumber))
)

# Plot the extimated function
grid = mesh(estimation$x1, estimation$x2)
extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, ray = ray, outValue = NA)
surf3D(x = grid$x, y = grid$y, z = estimation$fhat,
       xlim = c(min(estimation$x1),max(estimation$x1)), ylim = c(min(estimation$x2), max(estimation$x2)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Extimated function")

# Plot the true density function and the extimated one as flat matrixes. 
# Different values are identified by different colors
par(pty = "s") # Force a square plot
plotMatrix(title = "Real density function", matrix = Z, colorMap = rainbow(20))
plotMatrix(title = "Extimated density function", matrix = extimatedFunction, colorMap = rainbow(20))



