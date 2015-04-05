# This script cumputes a bidimensional gaussian distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# FALSE in the map means 'no fault' and TRUE means 'fault', whereas NA indicates 
# a point out of the circular wafer.

# Import required libraries
library(mvtnorm)  # Needed for dmvnorm()
library(KernSmooth) # Needed for KDE
library(KDEBenchmark) 

# Initial parameters
ray = 50
mu = c(ray, ray)
sigma = ray*matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
maximumFaultProbability = 0.5
bandwidth = seq(from = 1, to = 10, by = 0.2)
error = rep(x = 0, length.out = length(bandwidth))

#Calcuate f(x) for a large number of possible values for x1 and x2
x1 = seq(from = 0, to = 2*ray, length.out = 2*ray)
x2 = seq(from = 0, to = 2*ray, length.out = 2*ray)
Z = gaussianDensity(x1 = x1, x2 = x2, mu = mu, sigma = sigma)

for (i in 1 : length(bandwidth)){
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability, faultValue = 1, notFaultValue = 0)
  faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = -1)
  
  # KDE
  faultIndex = which(faultMap == 1, arr.ind = TRUE)
  estimation = bkde2D(faultIndex, bandwidth = bandwidth[i],  range.x = list(c(0,2*ray), c(0,2*ray)))
  
  # Benchmark
  trueFunction = gaussianDensity(x1 = estimation$x1, x2 = estimation$x2, mu = mu, sigma = sigma)
  error[i] = sum((trueFunction - estimation$fhat)^2)
}

# Plot the results
scatter2D(x = bandwidth, y = error)

