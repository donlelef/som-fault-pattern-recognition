# NOT TO BE USED: replace with MaxLikelihoodBandwidth

# This script computes a bidimensional probability distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distribution from the faults on the map using KDE algorithm.
# Different bandwidth are used and, for each number of faults, the best one is 
# saved. After that, the relationship between the amounts of faults and the 
# best bandwith is represented through a linear model.

# Import required libraries
library(KernSmooth) # Needed for bkde2D
library(KDEModel) # Needed for model
library(KDEPlotTools) # Needed for the plot
library(KDEBenchmark) # Needed for everything
library(KDEFaultPattern) # Needed for everything
library(stats) # Needed for lm

# Definition of execution parameters: fault probabilty functions
ray = 30
dieWidth = 1
dieHeight = 1
mu = c(ray, ray)
mu1 = c(ray, 50) # only for multiGaussianDensity
mu2 = c(10, ray) # only for multiGaussianDensity
mu3 = c(ray, 10) # only for multiGaussianDensity
sigma1 = ray*diag(x = c(1, 1))
sigma2 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity
sigma3 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity

# Definition of execution parameters: bandwidth limits
N_BAND = 50
lowerBandwidthLimit = 2
upperBandwidthLimit = 8

# Definition of execution parameters: amounts of faults
# gaussian: 0.05 - 0.3
# multiGaussian: 0.03 - 0.1
# parabolic: 0.02 - 0.06
N_PROB = 50
lowerMaximumFaultProbability = 0.05
upperMaximumFaultProbability = 0.3

# Initializations
maximumFaultProbabilities = seq(from = lowerMaximumFaultProbability, to = upperMaximumFaultProbability, length.out = N_PROB)
bandwidth = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = N_BAND)
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)
faultNumbers = vector(mode = "numeric", length = length(maximumFaultProbabilities))
fittedBandwidth = vector(mode = "numeric", length = length(maximumFaultProbabilities))

for(j in 1:length(maximumFaultProbabilities)){
  
  error = vector(mode = "numeric", length = length(bandwidth))
  
  # Calcuate f(x) for a large number of possible values for x1 and x2
  grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
   trueFunction = gaussianDensity(axes = grid, mu = mu, sigma = sigma1)$pdf
  # trueFunction = parabolicDensity(axes = grid, coefficient = 1, ray = ray)$pdf
  # trueFunction = multiGaussianDensity(axes = grid, parameterList = parameterList)$pdf
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = fillRectangularMap(probabilityFunction = trueFunction, maxFaultProbability = maximumFaultProbabilities[j], faultValue = 1, notFaultValue = 0)
  faultMap = bindCircularMap(rectangularMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = -1)
  faultNumbers[j] = faultNumber(faultMap = faultMap, faultValue = 1)
  
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
    extimatedFunction = bindCircularMap(rectangularMap = estimation$fhat, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
    
    # Benchmark
    error[i] = chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunction)
  }
  
  # Identify polynomial model and find the best model using AIC
  grades = 1:8
  bestFit = findBestModel(x = bandwidth, y = error, interval = c(min(grades), max(grades)))
  bestBandwidth = findMinimumFromModel(model = bestFit, interval = c(min(bandwidth), max(bandwidth)))$minimum
  fittedBandwidth[j] = bestBandwidth
}

# Plot the results
scatterPlot(x = faultNumbers, y = fittedBandwidth, title =  "Optimal bandwidth vs faults",
            sub = bquote("Number of simulations:"~.(length(faultNumbers))), 
            xlab = "Faults", ylab = "Bandwidth"
)

# Identify polynomial model
grades = 1:8
newData = seq(from = min(faultNumbers), to = max(faultNumbers), length.out = 250)
predictions = fitLinearModels(x = faultNumbers, y = fittedBandwidth, grades = grades, newData = newData)

# And plotting them
for(i in 1 : length(grades)){
  par(new = TRUE) # plot in the same graphic window
  modelPlot(x = newData, y = predictions[[i]],
            xlim = c(min(faultNumbers), max(faultNumbers)), 
            ylim = c(min(fittedBandwidth), max(fittedBandwidth)),
            col = rainbow(length(grades))[i]
  )
}

# Find the best model using AIC 
bestFit = findBestModel(x = faultNumbers, y = fittedBandwidth, interval = c(min(grades), max(grades)))
bestBandwidth = findMinimumFromModel(model = bestFit, interval = c(min(faultNumbers), max(faultNumbers)))$minimum
par(new = FALSE) # create a new plot
scatterPlot(x = faultNumbers, y = fittedBandwidth, title = "Optimal bandwidth vs faults", 
            sub = bquote("Simulations:"~.(length(faultNumbers))~"  Best rank:"~.(bestFit$rank - 1)),
            xlab = "Faults", ylab = "Bandwidth")
par(new = TRUE) # plot in the same graphic window
prediction = predict(bestFit, newdata =  data.frame(x = newData))
modelPlot(x = newData, y = prediction, col = "red",
          xlim = c(min(faultNumbers), max(faultNumbers)), ylim = c(min(fittedBandwidth), max(fittedBandwidth))
)

# Saving the data
data = data.frame(faultNumbers = faultNumbers, fittedBandwidth = fittedBandwidth)
saveRDS(data, "data.rds", ascii=TRUE)
