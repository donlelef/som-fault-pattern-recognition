# This script computes three different bidimensional probability distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distributions from the faults on the map using KDE algorithm.
# Different bandwidth are used and, for each number of faults, the best one is 
# saved. The best bandwidth is the one which causes the minimum error, defined as
# the sum of the extimation errors on the different distribution. The weight of the
# single distribution is arbitrary. 
# After that, the relationship between the amounts of faults and the 
# best bandwith is represented through a linear model.

# Import required libraries
library(KernSmooth) # Needed for bkde2D
library(KDEModel) # Needed for model
library(KDEPlotTools) # Needed for the plot
library(KDEBenchmark) # Needed for everything
library(stats) # Needed for lm

# Definition of execution parameters: fault probabilty functions
weigths = c(1,1,1,1)
ray = 30
mu = c(ray, ray)
mu1 = c(ray, 50) # only for multiGaussianDensity
mu2 = c(10, ray) # only for multiGaussianDensity
mu3 = c(ray, 10) # only for multiGaussianDensity
sigma1 = ray*diag(x = c(1, 1))
sigma2 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity
sigma3 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity

# Definition of execution parameters: bandwidth limits
N_BAND = 50
lowerBandwidthLimit = 2.5
upperBandwidthLimit = 8

# Definition of execution parameters: amounts of faults
minDefectNumber = 10
maxDefectNumber = 60

# Initializations
weigths = weigths/sum(weigths)
bandwidth = seq(from = lowerBandwidthLimit, to = upperBandwidthLimit, length.out = N_BAND)
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)
faultNumbers = seq(from = minDefectNumber, to = maxDefectNumber, by = 1)
fittedBandwidth = vector(mode = "numeric", length = length(faultNumbers))

# Calcuate f(x) for a large number of possible values for x1 and x2
# and fill a list with the four possible distributions
trueFunction1 = gaussianDensity(ray = ray, mu = mu, sigma = sigma1)$pdf
trueFunction2 = parabolicDensity(coefficient = 1, ray = ray)$pdf
trueFunction3 = multiGaussianDensity(ray = ray, parameterList = parameterList)$pdf
trueFunction4 = uniformDensity(ray = ray)$pdf
distributionsList = list(trueFunction1, trueFunction2, trueFunction3, trueFunction4)
for(i in 1:length(distributionsList)){
  distributionsList[[i]] = bindCircularMap(rectangularMap = distributionsList[[i]], ray = ray, outValue = 0)
}

for(j in 1:length(faultNumbers)){
  
  error = vector(mode = "numeric", length = length(bandwidth))
    
  for(k in 1:length(distributionsList)){
    # Select a distribution and its weight
    trueFunction = distributionsList[[k]]
    selectedWeight = weigths[k]
    
    # Fill a simulated wafer with good and bad chips according to the just computed density.
    faultMap = bindDefectNumber(probabilityMatrix = trueFunction, faultValue = 1, notFaultValue = 0, faultNumber = faultNumbers[j])
    
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
      error[i] = error[i] + selectedWeight*chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunction)
    }
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
