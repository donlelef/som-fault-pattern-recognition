# This script computes a bidimensional parabolic distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# 0 in the map means 'no fault' and 1 means 'fault', whereas NA indicates 
# a point out of the circular wafer.
# The KDE algorithm is lauched and the original function is predicted form the 
# positions of the defects on the wafer
# Five plot show the difference between the real function and the extimated one.

# Import required libraries
library(KernSmooth) # Needed for bkde2D
library(KDEPlotTools) # Needed for the plot
library(KDEFaultPattern) # Needed for everything
library(RColorBrewer) # Needed for plot
palette = rev(brewer.pal(11, "RdYlBu"))


# Initial parameters
dieWidth = 2
dieHeight = 1
ray = 30
faultNumber = 60
coefficient = 1
bandwidth = 10

# Create fault probability function
grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
list = parabolicDensity(axes = grid, coefficient = coefficient, ray = ray)
trueFunction = list$pdf
grid = list$grid

# Fill a simulated wafer with good and bad chips according to the just computed density
# and the chosen amount of faults.
faultMap = bindDefectNumber(probabilityMatrix = trueFunction, faultValue = 1, notFaultValue = 0, faultNumber = faultNumber)

# Perform the KDE
faultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)
estimation = bkde2D(faultPositions, bandwidth = bandwidth, 
                    range.x = list( c(min(grid$x), max(grid$x)), c(min(grid$y), max(grid$y))), 
                    gridsize = c(nrow(grid$x), ncol(grid$y)))

# 3D plot of the fault probability density with surf3D()
trueFunction = bindCircularMap(rectangularMap = trueFunction, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
surfacePlot(title = "Normalized parabolic distribution", x = grid$x, y = grid$y,  z = trueFunction)

# Plot the fault map
faultMap = bindCircularMap(rectangularMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
matrixPlot(title = "Fault map", matrix = faultMap, colorMap = heat.colors(2), 
           sub = bquote("Number of faults = "~.(faultNumber))
)

# Plot the extimated function
grid = mesh(estimation$x1, estimation$x2)
extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
surfacePlot(x = grid$x, y = grid$y, z = extimatedFunction, title = "Extimated function")

# Plot the true density function and the extimated one as flat matrixes. 
# Different values are identified by different colors
matrixPlot(title = "Real density function", matrix = trueFunction, colorMap = palette)
matrixPlot(title = "Extimated density function", matrix = extimatedFunction, colorMap = palette)

