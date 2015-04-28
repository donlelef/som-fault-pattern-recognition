# This script computes a bidimensional parabolic distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# FALSE in the map means 'no fault' and TRUE means 'fault', whereas NA indicates 
# a point out of the circular wafer.
# The KDE algorithm is lauched and the original function is predicted form the 
# positions of the defects on the wafer
# Five plot show the difference between the real function and the extimated one.

# Import required libraries
library(KernSmooth) # Needed for KDE
library(KDEBenchmark) # Needed for everything

# Initial parameters
ray = 30
maximumFaultProbability = 0.1
coefficient = 1
bandwidth = 11

#Calcuate f(x) for a large number of possible values for x1 and x2
list = parabolicDensity(coefficient = coefficient, ray = ray)
Z = list$pdf
grid = list$grid

# Fill the fault map
faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability, faultValue = TRUE, notFaultValue = FALSE)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = NA)

# Compute the fault number
faultNumber = faultNumber(faultMap = faultMap, faultValue = TRUE)

# Perform the KDE
faultIndex = which(faultMap == 1, arr.ind = TRUE)
estimation = bkde2D(x = faultIndex, bandwidth = bandwidth, range.x = list(c(0,2*ray), c(0, 2*ray)), gridsize = c(2*ray, 2*ray))

# 3D plot of the fault probability density with surf3D()
Z = bindCircularMap(rectangularMap = Z, ray = ray, outValue = NA)
plotSurface(title = "Normalized parabolic distribution", x = grid$x, y = grid$y,  z = Z)

# Plot the fault map
par(pty = "s") # Force a square plot
plotMatrix(title = "Fault map", matrix = faultMap, colorMap = heat.colors(2), 
           sub = bquote("Number of faults = "~.(faultNumber))
)

# Plot the extimated function
grid = mesh(estimation$x1, estimation$x2)
extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, ray = ray, outValue = NA)
plotSurface(x = grid$x, y = grid$y, z = estimation$fhat, title = "Extimated function")

# Plot the true density function and the extimated one as flat matrixes. 
# Different values are identified by different colors
par(pty = "s") # Force a square plot
plotMatrix(title = "Real density function", matrix = Z, colorMap = rainbow(20))
plotMatrix(title = "Extimated density function", matrix = extimatedFunction, colorMap = rainbow(20))

