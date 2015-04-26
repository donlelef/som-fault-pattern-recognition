# This script cumputes a bidimensional gaussian distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# FALSE in the map means 'no fault' and TRUE means 'fault', whereas NA indicates 
# a point out of the circular wafer.

# Import required libraries
library(plot3D)   # Needed for mesh()
library(KernSmooth) # Needed for bkde2D
library(KDEBenchmark) 

# Initial parameters
ray = 30
maximumFaultProbability = 0.01
bandwidth = 50

# Calcuate f(x) for a large number of possible values for x1 and x2
x1 = x2 = seq(from = 0, to = 2*ray, length.out = 2*ray)
Z = (1/(pi*ray^2))*matrix(data = 1, nrow = 2*ray, ncol = 2*ray)
grid = mesh(x1, x2)

# Fill a simulated wafer with good and bad chips according to the just computed density.
faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray)

# Compute the fault number
faultNumber = faultNumber(faultMap = faultMap, faultValue = TRUE)

# Perform the KDE
faultIndex = which(faultMap == 1, arr.ind = TRUE)
estimation = bkde2D(faultIndex, bandwidth = bandwidth, range.x = list(c(0,2*ray), c(0,2*ray)), gridsize = c(2*ray, 2*ray))

# 3D plot of the fault probability density with surf3D()
Z = bindCircularMap(rectangularMap = Z, ray = ray, outValue = NA)
surf3D(x = grid$x, y = grid$y, z = Z,  
       xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       colkey = FALSE, main = "Uniform distribution"
)

# Plot the fault map
par(pty = "s") # Force a square plot
plotMatrix(title = "Simulated fault map", matrix = faultMap, colorMap = heat.colors(2),
           sub = bquote("Number of faults = "~.(faultNumber))
)

# Plot the extimated function
grid = mesh(estimation$x1, estimation$x2)
extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, ray = ray, outValue = NA)
surf3D(x = grid$x, y = grid$y, z = extimatedFunction,
       xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2)), zlim = c(0, max(extimatedFunction, na.rm = TRUE)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       colkey = FALSE, main = "Extimated function"
)

# Plot the true density function and the extimated one as flat matrixes. 
# Different values are identified by different colors
par(pty = "s") # Force a square plot
plotMatrix(title = "Real density function", matrix = Z, colorMap = rainbow(20))
plotMatrix(title = "Extimated density function", matrix = extimatedFunction, colorMap = rainbow(20))