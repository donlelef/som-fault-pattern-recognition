# This script computes a bidimensional probability distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distribution from the faults on the map using KDE algorithm.
# The benchmark concern the accuracy of the estimation as the bandwidth for
# the KDE algorithm ranges in a given interval.
# The script finds the best bandwidth, ie the one which causes the minumium difference
# between the real probability function and the extimated one.

# Import required libraries
library(KDEModel) # Needed for model
library(KDEPlotTools) # Needed for the plot
library(KDEBenchmark) # Needed for everything
library(KDEFaultPattern) # Needed for everything
library(KernSmooth) # Needed for bkde2D
library(stats) # Needed for predict

# initialize parameters
source(file = "Script//initializer.R")

# Definition of execution parameters
N_BAND = 100
faultNumber = 10
lowerBandwidthLimit = 2
upperBandwidthLimit = 6

# Initializations
bandwidth = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = N_BAND)
error = vector(mode = "numeric", length = length(bandwidth))

# Fill a map of faulty and good chips according to one of the possible distributions
# and the chosen amounts of faults
trueFunction = distributionsList$gaussian
faultMap = bindDefectNumber(probabilityMatrix = trueFunction, faultValue = 1, notFaultValue = 0, faultNumber = faultNumber)
  
# KDE: finding the fault position
faultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)

# Consider only the points inside the wafer
trueFunction = bindCircularMap(rectangularMap = trueFunction, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)

# Repeat the simulation for several values of bandwidth
for (i in 1 : length(bandwidth)){  
  # KDE
  estimation = bkde2D(faultPositions, bandwidth = bandwidth[i], 
                      range.x = list( c(min(grid$x), max(grid$x)), c(min(grid$y), max(grid$y))), 
                      gridsize = c(length(grid$x), length(grid$y)))
  
  # Consider only the points inside the wafer
  extimatedMatrix = bindCircularMap(rectangularMap = estimation$fhat, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
  
  # Benchmark
  error[i] = chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedMatrix, na.rm = TRUE)
}

# Plot the results
scatterPlot(x = bandwidth, y = error, title =  "Average square error vs bandwidth",
            sub = bquote("Number of simulations:"~.(length(bandwidth))), 
            xlab = "bandwidth", ylab = "error"
)

# Identify polynomial models and fit date according to them
grades = 1:8
newData = seq(from = min(bandwidth), to = max(bandwidth), length.out = 250)
predictions = fitLinearModels(x = bandwidth, y = error, grades = grades, newData = newData)

# Plot all the models upon the data scatter plot
for(i in 1 : length(grades)){
  par(new = TRUE) # plot in the same graphic window
  modelPlot(x = newData, y = predictions[[i]],
            xlim = c(min(bandwidth), max(bandwidth)), ylim = c(min(error), max(error)),
            col = rainbow(length(grades))[i]
  )
}

# Find the best model using AIC 
bestFit = findBestModel(x = bandwidth, y = error, interval = c(min(grades), max(grades)))
bestBandwidth = findMinimumFromModel(model = bestFit, interval = c(min(bandwidth), max(bandwidth)))$minimum
par(new = FALSE) # create a new plot
scatterPlot(x = bandwidth, y = error, title = "Average square error vs bandwidth", 
            sub = bquote("Simulations:"~.(length(bandwidth))~
                           "  Best rank:"~.(bestFit$rank - 1)~"   Best bandwidth:"~.(bestBandwidth)),
            xlab = "bandwidth", ylab = "error")

# Plot the best model on the data
par(new = TRUE) # plot in the same graphic window
prediction = predict(bestFit, newdata =  data.frame(x = newData))
modelPlot(x = newData, y = prediction, col = "red",
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error))
)
