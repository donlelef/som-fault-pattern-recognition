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

# Initialize wafer parameters and distributions
source(file = "Script//initializer.R")
distributionsList = list(distributionsList$gaussian, distributionsList$parabolic, distributionsList$multiGaussian)

# Initilize test parameters
faultNumbers = seq(from = 10, to = 60, by = 1)
KernError = KsError = vector(mode = "numeric", length = length(faultNumbers))

# load the bandwit curve from file
dataFrame = readRDS(file = "Data//maxLikelihoodBestBandwidths.rds")
fittedFaults = dataFrame$faults
fittedBandwidth = dataFrame$bestBand

for(j in 1:length(distributionsList)){
  
  for(i in 1:length(faultNumbers)){
    
    # Select the fault probability distribution
    trueFunction = distributionsList[[j]]
    
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
    KernError[i] = KernError[i] + chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunctionKern)
    KsError[i] = KsError[i] + chiTest(trueMatrix = trueFunction, extimatedMatrix = extimatedFunctionKs)
  }
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
