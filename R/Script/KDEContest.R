# The script creates an arbitrary number of wafers with a small amount of faults and
# then performs the Kernel Density Estimation using both KernSmooth and Ks functions.
# The returned fault probability distribution is compared to the real one and the
# mean square error is computed. 

# Import required libraries
library(ks) # needed for kde
library(KernSmooth) # Needed for bkde2D
library(KDEBenchmark) # Needed for everything
library(KDEPlotTools) # Needed for the plot

# Initilize test parameters
tests = 50
resolution = 50
gaussianFaultsProbability = seq(from = 0.05, to = 0.3, length.out = resolution)
parabolicFaultsProbability = seq(from = 0.03, to = 0.1, length.out = resolution)
multiGaussianFaultsProbability = seq(from = 0.02, to = 0.06, length.out = resolution)

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
fittedFaults = dataFrame$faults
fittedBandwidth = dataFrame$bestBand

for(i in 1:tests){
  
  # Calcuate the probability distribution: choose one!
  # trueFunction = gaussianDensity(ray = ray, mu = mu1, sigma = sigma1)$pdf
  # maxFaultProb = gaussianFaultsProbability[i]
  
  # trueFunction = parabolicDensity(coefficient = 1, ray = ray)$pdf
  # maxFaultProb = parabolicFaultsProbability[i]
  
  #  trueFunction = multiGaussianDensity(ray = ray, parameterList = parameterList)$pdf
  #  maxFaultProb = multiGaussianFaultsProbability[i]
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = fillRectangularMap(probabilityFunction = trueFunction, maxFaultProbability = maxFaultProb, faultValue = 1, notFaultValue = 0)
  faultMap = bindCircularMap(rectangularMap = faultMap, ray = ray, outValue = -1)
  
  # Find the fault position and their number
  faultIndex = which(faultMap == 1, arr.ind = TRUE)
  faultNumbers[i] = faultNumber(faultMap = faultMap, faultValue = 1)
  
  # Consider only the points inside the wafer
  trueFunction = bindCircularMap(rectangularMap = trueFunction, ray = ray, outValue = NA)
  
  # KDE with KernSmooth and with ks
  bestBandwidth = bestBandwidth(fittedBandwidth = fittedBandwidth, fittedFaults = fittedFaults, faults = faultNumbers[i]) 
  estimationKern = bkde2D(x = faultIndex, bandwidth = bestBandwidth,  range.x = list(c(0,2*ray), c(0,2*ray)), gridsize = c(2*ray, 2*ray))
  estimationKs = kde(x = faultIndex, gridsize = c(2*ray, 2*ray), xmin = c(0,0), xmax = c(2*ray, 2*ray))
  
  # Consider only the points inside the wafer
  extimatedFunctionKern = bindCircularMap(rectangularMap = estimationKern$fhat, ray = ray, outValue = NA)
  extimatedFunctionKs = bindCircularMap(rectangularMap = estimationKs$estimate, ray = ray, outValue = NA)
  
  # Benchmark
  KernError[i] = chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunctionKern)
  KsError[i] = chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunctionKs)
}

# Comupting difference
diff = KsError - KernError

# Scatter plots
par(new = FALSE) # create a new plot
scatterPlot(x = faultNumbers, y = KernError, title = "Average square error vs faults", 
            sub = bquote("Simulations:"~.(length(faultNumbers))), col = "blue", 
            xlim = c(min(faultNumbers),max(faultNumbers)), ylim = c(min(KernError, KsError), max(KernError, KsError)),
            xlab = "Faults", ylab = "Error")
par(new = TRUE) # plot in the same graphic window
scatterPlot(x = faultNumbers, y = KsError, title = "Average square error vs faults",
            xlim = c(min(faultNumbers),max(faultNumbers)), ylim = c(min(KernError, KsError), max(KernError, KsError)),
            col = "red", xlab = "", ylab = "", axes = FALSE)

# Plot difference
par(new = FALSE) # create a new plot
barplot(height = diff, xaxs="i", col=ifelse(test = diff>0, yes = "blue", no = "red"),
        main = "Our error vs ks error", ylim = c(min(diff), max(diff)), 
        xlab = "", ylab = "Difference", sub = bquote("Simulations:"~.(length(faultNumbers)))
        )
