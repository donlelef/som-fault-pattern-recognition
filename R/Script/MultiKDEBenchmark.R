# This script computes fuor different bidimensional probability distributions,
# uses it as probability functions to create faultMaps, and then tries to estimate
# the original distributions from the faults on the map using KDE algorithm.
# The extimation is based on the maximum likelihood method on the minimization 
# of the error between the real distribution and the extimated one.
# The benchmark concern the accuracy of the estimation as the bandwidth for
# the KDE algorithm ranges in a given interval.

# Import required libraries
library(KernSmooth) # Needed for bkde2D
library(KDEModel) # Needed for model
library(KDEPlotTools) # Needed for the plot
library(KDEBenchmark) # Needed for everything
library(KDEFaultPattern) # Needed for everything
library(stats) # Needed for predict

# Definition of execution parameters
weigths = c(1,1,1,1)
ray = 30
dieWidth = 1
dieHeight = 1
faultNumber = 20
mu = c(ray, ray)
mu1 = c(ray, 50)
mu2 = c(10, ray) # only for miltiGaussianDensity
mu3 = c(ray, 10) # only for miltiGaussianDensity
sigma1 = ray*diag(x = c(1, 1))
sigma2 = ray*diag(x = c(1, 1)) # only for miltiGaussianDensity
sigma3 = ray*diag(x = c(1, 1)) # only for miltiGaussianDensity
N_BAND = 50
maximumFaultProbability = 0.1
lowerBandwidthLimit = 2
upperBandwidthLimit = 6

# Initializations
weigths = weigths/sum(weigths)
bandwidth = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = N_BAND)
error = vector(mode = "numeric", length = length(bandwidth))
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)

# Calcuate f(x) for a large number of possible values for x1 and x2
# and fill a list with the fuor possible distributions
grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
trueFunction1 = gaussianDensity(axes = grid, mu = mu, sigma = sigma1)$pdf
trueFunction2 = parabolicDensity(axes = grid, coefficient = 1, ray = ray)$pdf
trueFunction3 = multiGaussianDensity(axes = grid, parameterList = parameterList)$pdf
trueFunction4 = uniformDensity(axes = grid)$pdf
distributionsList = list(trueFunction1, trueFunction2, trueFunction3, trueFunction4)

for(j in 1:length(distributionsList)){
  # Select a distribution and its weight
  trueFunction = distributionsList[[j]]
  selectedWeight = weigths[j]
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  trueFunction = bindCircularMap(rectangularMap = trueFunction, dieWidth = dieWidth, dieHeight = dieHeight,  waferRay = ray, outValue = 0)
  faultMap = bindDefectNumber(probabilityMatrix = trueFunction, faultValue = 1, notFaultValue = 0, faultNumber = 20)
  
  # KDE: finding the fault position
  faultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)
  
  # Consider only the points inside the wafer
  trueFunction = bindCircularMap(rectangularMap = trueFunction, dieWidth = dieWidth, dieHeight = dieHeight,  waferRay = ray, outValue = NA)
  
  # Repeat the simulation for several values of bandwidth
  for (i in 1 : length(bandwidth)){  
    # KDE
    estimation = bkde2D(faultPositions, bandwidth = bandwidth[i], 
                        range.x = list( c(min(grid$x), max(grid$x)), c(min(grid$y), max(grid$y))), 
                        gridsize = c(length(grid$x), length(grid$y)))
    
    # Consider only the points inside the wafer
    extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, dieWidth = dieWidth, dieHeight = dieHeight,  waferRay = ray, outValue = NA)
    
    # Benchmark
    error[i] = error[i] + selectedWeight*chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunction, na.rm = TRUE) 
  }
}

# Plot the results
scatterPlot(x = bandwidth, y = error, title =  "Average square error vs bandwidth",
            sub = bquote("Number of simulations:"~.(length(bandwidth))), 
            xlab = "bandwidth", ylab = "error"
)

# Identify polynomial model
grades = 1:8
newData = seq(from = min(bandwidth), to = max(bandwidth), length.out = 250)
predictions = fitLinearModels(x = bandwidth, y = error, grades = grades, newData = newData)

# And plotting them
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
par(new = TRUE) # plot in the same graphic window
prediction = predict(bestFit, newdata =  data.frame(x = newData))
modelPlot(x = newData, y = prediction, col = "red",
          xlim = c(min(bandwidth),max(bandwidth)), ylim = c(min(error), max(error))
)
