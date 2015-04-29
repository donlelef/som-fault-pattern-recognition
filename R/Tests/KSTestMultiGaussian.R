# ks test

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
library(ks) # Needed for kde
library(KDEPlotTools) # Needed for the plot
library(KDEBenchmark) # Needed for everything

# Initial parameters
ray = 30
sigma1 = ray*matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
sigma2 = ray*matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
parameterList = list(list(mu = c(ray,ray), sigma = sigma2), 
                     list(mu = c(10,ray), sigma = sigma1),
                     list(mu = c(ray,10), sigma = sigma1)                     
)
maximumFaultProbability = 0.1

# Calcuate f(x) for a large number of possible values for x1 and x2
list = multiGaussianDensity(ray = ray, parameterList = parameterList)
Z = list$pdf
grid = list$grid

# Fill a simulated wafer with good and bad chips according to the just computed density.
faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray)

# Compute the fault number
faultNumber = faultNumber(faultMap = faultMap, faultValue = TRUE)

# Perform the KDE
faultIndex = which(faultMap == 1, arr.ind = TRUE)
estimation = kde(x = faultIndex, xmin = c(0 ,0), xmax = c(2*ray ,2*ray), gridsize = c(2*ray, 2*ray))
# estimation = bkde2D(faultIndex, bandwidth = bandwidth, range.x = list(c(0,2*ray), c(0,2*ray)), gridsize = c(2*ray, 2*ray))

# 3D plot of the fault probability density with surf3D()
Z = bindCircularMap(rectangularMap = Z, ray = ray, outValue = NA)
plotSurface(x = grid$x, y = grid$y, z = Z, title = "Bivariate Normal Distribution",
            sub = bquote(bold(mu[1])==.(mu[1])~", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~", "~sigma[2]==.(sigma[2,2])~", "~sigma[xy]==.(sigma[2,1]))
)

# Plot the fault map
plotMatrix(title = "Simulated fault map", matrix = faultMap, colorMap = heat.colors(2),
           sub = bquote("Number of faults = "~.(faultNumber))
)

# Plot the extimated function
grid = mesh(estimation$eval.points[[1]], estimation$eval.points[[2]])
extimatedFunction = bindCircularMap(rectangularMap = estimation$estimate, ray = ray, outValue = NA)
plotSurface(x = grid$x, y = grid$y, z = extimatedFunction, title = "Extimated function",
            sub = bquote(bold(mu[1])==.(mu[1])~", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~", "~sigma[2]==.(sigma[2,2])~", "~sigma[xy]==.(sigma[2,1]))
)

# Plot the true density function and the extimated one as flat matrixes. 
# Different values are identified by different colors
plotMatrix(title = "Real density function", matrix = Z, colorMap = rainbow(20))
plotMatrix(title = "Extimated density function", matrix = extimatedFunction, colorMap = rainbow(20))
