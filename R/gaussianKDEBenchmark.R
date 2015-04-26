# This script cumputes a bidimensional gaussian distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distribution from the faults on the map using KDE algorithm.
# The benchmark concern the accuracy of the estimation as the bandwidth for
# the KDE algorithm ranges in the interval [1, 10].

# Import required libraries
library(stats) # Needed for lm
library(KernSmooth) # Needed for KDE
library(plot3D) # Needed to plot the results with scatter2D function
library(KDEBenchmark) 

# Definition of execution parameters
ray = 50
sigma1 = 1
sigma2 = 1
N_BAND = 50
maximumFaultProbability = 0.05

# Initializations
mu = c(ray, ray)
sigma = ray*diag(x = c(sigma1, sigma2))
bandwidth = seq(from = 1.5, to = 9, length.out = N_BAND)
error = rep_len(x = 0, length.out = length(bandwidth))

# Calcuate f(x) for a large number of possible values for x1 and x2
x1 = seq(from = 0, to = 2*ray, length.out = 2*ray)
x2 = seq(from = 0, to = 2*ray, length.out = 2*ray)
Z = gaussianDensity(x1 = x1, x2 = x2, mu = mu, sigma = sigma)$pdf

# Repeat the simulation for several values of bandwidth
for (i in 1 : length(bandwidth)){
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = fillRectangularMap(probabilityFunction = Z, maxFaultProbability = maximumFaultProbability, faultValue = 1, notFaultValue = 0)
  faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = -1)
  
  # KDE
  faultIndex = which(faultMap == 1, arr.ind = TRUE)
  estimation = bkde2D(x = faultIndex, bandwidth = bandwidth[i],  range.x = list(c(0,2*ray), c(0,2*ray)))
  
  # Benchmark
  trueFunction = gaussianDensity(x1 = estimation$x1, x2 = estimation$x2, mu = mu, sigma = sigma)$pdf
  error[i] = sum((trueFunction - estimation$fhat)^2)
}

# Plot the results
scatter2D(x = bandwidth, y = error, pch = 4, 
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(0, max(error)),
          main = "Average square error vs bandwidth",
          sub = bquote("Number of simulations:"~.(length(bandwidth))), xlab = "bandwidth",
          ylab = "error")

# Identify polynomial model
maximumGrade = 8
newData = seq(from = 1, to = 10, length.out = 250)
for(i in 1 : maximumGrade){
  fit = lm(error~poly(bandwidth, i))
  par(new = TRUE) # plot in the same graphic window
  plot(x = newData, y = predict(fit, data.frame(bandwidth = newData)),
       xlim = c(min(bandwidth),max(bandwidth)), ylim = c(0, max(error)),
       type = "l", col = rainbow(maximumGrade)[i], xlab = "", ylab = "", axes = FALSE)
}

# Find the best model using AIC
polyfit = function(i) x = AIC(lm(error~poly(bandwidth,i)))
best = as.integer(optimize(f = polyfit, interval = c(1,maximumGrade), maximum = FALSE)$minimum)

# Consider only the best model and find the best bandwidth - 
# ie. the one which causes the minimun error 
bestFit = lm(error~poly(bandwidth, best))
modelValues = function(i) x = predict(bestFit, newdata = data.frame(bandwidth = i))
bestBandwidth = optimize(f = modelValues,interval = c(1,max(bandwidth)), maximum = FALSE)$minimum
par(new = FALSE) # create a new plot
scatter2D(x = bandwidth, y = error, pch = 4, 
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(0, max(error)),
          main = "Average square error vs bandwidth",
          sub = bquote("Simulations:"~.(length(bandwidth))~
                         "  Best grade:"~.(best)~"   Best bandwidth:"~.(bestBandwidth)),
          xlab = "bandwidth", ylab = "error")
par(new = TRUE) # plot in the same graphic window
plot(x = newData, y = predict(bestFit, newdata =  data.frame(bandwidth = newData)),
     xlim = c(min(bandwidth),max(bandwidth)), ylim = c(0, max(error)),
     type = "l", col = rainbow(maximumGrade)[i], xlab = "", ylab ="", axes = FALSE)

# TODO: extract method on the second part of the script
