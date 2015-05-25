# The script creates an arbitrary number of wafers with a small amount of faults and
# then performs the Kernel Density Estimation using both KernSmooth and Ks functions.
# The returned fault probability distribution is compared to the real one and the
# mean square error is computed. 

# Import required libraries
library(ks) # needed for kde
library(KernSmooth) # Needed for bkde2D
library(KDEBenchmark) # Needed for everything
library(KDEFaultPattern) # Needed for everything
library(KDEPlotTools) # Needed for the plot

# Initilize test parameters
faultNumbers = seq(from = 10, to = 60, by = 1)

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
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)

# Calcuate f(x) for a large number of possible values for x1 and x2
# and fill a list with the three possible distributions
grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
trueFunction1 = gaussianDensity(axes = grid, mu = mu, sigma = sigma1)$pdf
trueFunction2 = parabolicDensity(axes = grid, coefficient = 1, ray = ray)$pdf
trueFunction3 = multiGaussianDensity(axes = grid, parameterList = parameterList)$pdf
distributionsList = list(trueFunction1, trueFunction2, trueFunction3)
for(i in 1:length(distributionsList)){
  distributionsList[[i]] = bindCircularMap(rectangularMap = distributionsList[[i]], dieWidth = dieWidth, dieHeight = dieHeight,  waferRay = ray, outValue = 0)
}

# Initializations
KernError = KsError = vector(mode = "numeric", length = length(faultNumbers))

# load the bandwit curve from file
dataFrame = readRDS(file = "data.rds")
fittedFaults = dataFrame$faults
fittedBandwidth = dataFrame$bestBand

for(i in 1:length(faultNumbers)){
  
  # Calcuate the probability distribution: choose one!
  trueFunction = distributionsList[[1]]
  
  # Fill a simulated wafer with good and bad chips according to the just computed density.
  faultMap = bindDefectNumber(probabilityMatrix = trueFunction, faultValue = 1, notFaultValue = 0, faultNumber = faultNumbers[i])
  
  # Find the fault position and their number
  faultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)
  
  # Consider only the points inside the wafer
  trueFunction = bindCircularMap(rectangularMap = trueFunction, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
  
  # KDE with KernSmooth and with ks
  bestBandwidth = bestBandwidth(fittedBandwidth = fittedBandwidth, fittedFaults = fittedFaults, faults = faultNumbers[i]) 
  estimationKern = bkde2D(faultPositions, bandwidth = bestBandwidth, 
                      range.x = list( c(min(grid$x), max(grid$x)), c(min(grid$y), max(grid$y))), 
                      gridsize = c(length(grid$x), length(grid$y)))
  estimationKs = kde(x = faultPositions, gridsize = c(length(grid$x), length(grid$y)), xmin = c(min(grid$x), min(grid$y)), xmax = c(max(grid$x), max(grid$y)))
  
  # Consider only the points inside the wafer
  extimatedFunctionKern = bindCircularMap(rectangularMap = estimationKern$fhat, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
  extimatedFunctionKs = bindCircularMap(rectangularMap = estimationKs$estimate, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, outValue = NA)
  
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
