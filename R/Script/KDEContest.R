# WORKING, BUT HEAVY REFACTOR NEEDED!!!!!!!!!!!!!!!

# Import required libraries
library(ks) # needed for kde
library(KernSmooth) # Needed for bkde2D
library(KDEBenchmark) # Needed for model
library(KDEModelIdentification) # Needed for model
library(KDEPlotTools) # Needed for the plot
library(stats) # Needed for predict
library(plot3D)

# Initilize test parameters
tests = 50
resolution = 50
gaussianFaultsProbability = seq(from = 0.05, to = 0.2, length.out = resolution)
parabolicFaultsProbability = seq(from = 0.03, to = 0.1, length.out = resolution)
multiGaussianFaultsProbability = seq(from = 0.02, to = 0.05, length.out = resolution)

# Definition of parameters for fault probability distributions
ray = 30
mu1 = c(ray, ray)
mu2 = c(10, ray) # only for multiGaussianDensity
mu3 = c(ray, 10) # only for multiGaussianDensity
sigma1 = ray*diag(x = c(1, 1))
sigma2 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity
sigma3 = ray*diag(x = c(1, 1)) # only for multiGaussianDensity
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)
                     )   # only for multiGaussianDensity

# Initializations
KernError = vector(mode = "numeric", length = tests)
KsError = vector(mode = "numeric", length = tests)
faultNumbers = vector(mode = "numeric", length = tests)

# load the bandwit curve from file
dataFrame = readRDS(file = "Data/bestBandwidths.rds")
fittedBandwidth = dataFrame$bestBand

for(i in 1:tests){
  
  # Calcuate the probability distribution
   trueFunction = gaussianDensity(ray = ray, mu = mu1, sigma = sigma1)$pdf
  # trueFunction = parabolicDensity(coefficient = 1, ray = ray)$pdf
  # trueFunction = multiGaussianDensity(ray = ray, parameterList = parameterList)$pdf
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = fillRectangularMap(probabilityFunction = trueFunction, maxFaultProbability = gaussianFaultsProbability[i], faultValue = 1, notFaultValue = 0)
  faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = -1)
  
  # Find the fault position and their number
  faultIndex = which(faultMap == 1, arr.ind = TRUE)
  faultNumbers[i] = faultNumber(faultMap = faultMap, faultValue = 1)
  
  # Consider only the points inside the wafer
  trueFunction = bindCircularMap(rectangularMap = trueFunction, ray = ray, outValue = NA)
  
  # KDE with KernSmooth and with ks
  if(faultNumbers[i] <= 10){
    bestBandwidth = min(fittedBandwidth)
  } else if(faultNumbers[i] >= 60){
    bestBandwidth = max(fittedBandwidth)
  } else {
    bestBandwidth = fittedBandwidth[faultNumbers[i] - 10]
  }
 
  estimationKern = bkde2D(x = faultIndex, bandwidth = bestBandwidth,  range.x = list(c(0,2*ray), c(0,2*ray)), gridsize = c(2*ray, 2*ray))
  estimationKs = kde(x = faultIndex, gridsize = c(2*ray, 2*ray), xmin = c(0,0), xmax = c(2*ray, 2*ray))
  
  # Consider only the points inside the wafer
  extimatedFunctionKern = bindCircularMap(rectangularMap = estimationKern$fhat, ray = ray, outValue = NA)
  extimatedFunctionKs = bindCircularMap(rectangularMap = estimationKs$estimate, ray = ray, outValue = NA)
  
  # Benchmark
  KernError[i] = meanSquareError(matrix1 = trueFunction, matrix2 = extimatedFunctionKern)
  KsError[i] = meanSquareError(matrix1 = trueFunction, matrix2 = extimatedFunctionKs)
}

# Comupting difference
diff = KsError - KernError

# Identify polynomial models
grades = 1:8
newData = seq(from = min(faultNumbers), to = max(faultNumbers), length.out = 250)
predictions = fitLinearModels(x = faultNumbers, y = diff, grades = grades, newData = newData)

# Find the best model using AIC 
bestFit = findBestModel(x = faultNumbers, y = diff, interval = c(min(grades), max(grades)))

# Scatter plots
par(new = FALSE) # create a new plot
scatterPlot(x = faultNumbers, y = KernError, title = "Average square error vs faults", 
            sub = bquote("Simulations:"~.(length(faultNumbers))), col = "red", 
            xlim = c(min(faultNumbers),max(faultNumbers)), ylim = c(min(KernError, KsError), max(KernError, KsError)),
            xlab = "Fault", ylab = "Error")
par(new = TRUE) # plot in the same graphic window
scatterPlot(x = faultNumbers, y = KsError, title = "Average square error vs faults",
            xlim = c(min(faultNumbers),max(faultNumbers)), ylim = c(min(KernError, KsError), max(KernError, KsError)),
            col = "blue", xlab = "", ylab = "", axes = FALSE)


# Plot difference
prediction = predict(bestFit, newdata = data.frame(x = newData))
par(new = FALSE) # create a new plot
plot(x = newData, y = prediction, main = "Our error vs ks error", type = "l",
            xlim = c(min(newData),max(newData)), ylim = c(min(prediction), max(prediction)),
            sub = bquote("Simulations:"~.(length(faultNumbers))), col = "blue", 
            xlab = "Difference", ylab = "Faults")







