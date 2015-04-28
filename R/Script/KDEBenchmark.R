# This script cumputes a bidimensional probability distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distribution from the faults on the map using KDE algorithm.
# The benchmark concern the accuracy of the estimation as the bandwidth for
# the KDE algorithm ranges in a given interval.

# Import required libraries
library(stats) # Needed for lm
library(KernSmooth) # Needed for KDE
library(plot3D) # Needed to plot the results with scatter2D function
library(KDEBenchmark) 

# Definition of execution parameters
ray = 50
mu1 = c(ray, ray)
mu2 = c(10, ray) # only for miltiGaussianDensity
mu3 = c(ray, 10) # only for miltiGaussianDensity
sigma1 = ray*diag(x = c(1, 1))
sigma2 = ray*diag(x = c(1, 1)) # only for miltiGaussianDensity
sigma3 = ray*diag(x = c(1, 1)) # only for miltiGaussianDensity
N_BAND = 100
maximumFaultProbability = 0.2
lowerBandwidthLimit = 2
upperBandwidthLimit = 10

# Initializations
bandwidth = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = N_BAND)
error = vector(mode = "numeric", length = length(bandwidth))
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)

# Calcuate f(x) for a large number of possible values for x1 and x2
# trueFunction = gaussianDensity(ray = ray, mu = mu1, sigma = sigma1)$pdf
# trueFunction = parabolicDensity(coefficient = 1, ray = ray)$pdf
 trueFunction = multiGaussianDensity(ray = ray, parameterList = parameterList)$pdf

# Fill a simulated wafer with good and bad chips according to the just computed density.
faultMap = fillRectangularMap(probabilityFunction = trueFunction, maxFaultProbability = maximumFaultProbability, faultValue = 1, notFaultValue = 0)
faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = -1)

# KDE: finding the fault position
faultIndex = which(faultMap == 1, arr.ind = TRUE)

# Consider only the points inside the wafer
trueFunction = bindCircularMap(rectangularMap = trueFunction, ray = ray, outValue = NA)

# Repeat the simulation for several values of bandwidth
for (i in 1 : length(bandwidth)){  
  # KDE
  estimation = bkde2D(x = faultIndex, bandwidth = bandwidth[i],  range.x = list(c(0,2*ray), c(0,2*ray)), gridsize = c(2*ray, 2*ray))
  
  # Consider only the points inside the wafer
  extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, ray = ray, outValue = NA)
  
  # Benchmark
  error[i] = sum((trueFunction - estimation$fhat)^2, na.rm = TRUE)
}

# Plot the results
scatter2D(x = bandwidth, y = error, pch = 4, 
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error)),
          main = "Average square error vs bandwidth",
          sub = bquote("Number of simulations:"~.(length(bandwidth))), xlab = "bandwidth",
          ylab = "error")

# Identify polynomial model
maximumGrade = 8
newData = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = 250)
for(i in 1 : maximumGrade){
  fit = lm(error~poly(bandwidth, i))
  par(new = TRUE) # plot in the same graphic window
  plot(x = newData, y = predict(fit, data.frame(bandwidth = newData)),
       xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error)),
       type = "l", col = rainbow(maximumGrade)[i], xlab = "", ylab = "", axes = FALSE)
}

# Find the best model using AIC
polyfit = function(i) x = AIC(lm(error~poly(bandwidth,i)))
best = as.integer(optimize(f = polyfit, interval = c(1,maximumGrade), maximum = FALSE)$minimum)

# Consider only the best model and find the best bandwidth - 
# ie. the one which causes the minimun error 
bestFit = lm(error~poly(bandwidth, best))
modelValues = function(i) x = predict(bestFit, newdata = data.frame(bandwidth = i))
bestBandwidth = optimize(f = modelValues,interval = c(min(bandwidth),max(bandwidth)), maximum = FALSE)$minimum
par(new = FALSE) # create a new plot
scatter2D(x = bandwidth, y = error, pch = 4, 
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error)),
          main = "Average square error vs bandwidth",
          sub = bquote("Simulations:"~.(length(bandwidth))~
                         "  Best grade:"~.(best)~"   Best bandwidth:"~.(bestBandwidth)),
          xlab = "bandwidth", ylab = "error")
par(new = TRUE) # plot in the same graphic window
plot(x = newData, y = predict(bestFit, newdata =  data.frame(bandwidth = newData)),
     xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error)),
     type = "l", col = rainbow(maximumGrade)[i], xlab = "", ylab ="", axes = FALSE)

# TODO: extract method on the second part of the script
