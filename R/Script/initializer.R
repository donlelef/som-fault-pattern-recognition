# Initilize parameter and create the four fault probability distributions

# Import required libraries
library(KDEFaultPattern) # Needed for everything

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

# Initializations
parameterList = list(list(mu = mu1, sigma = sigma1), 
                     list(mu = mu2, sigma = sigma2),
                     list(mu = mu3, sigma = sigma3)                    
)

# Calcuate f(x) for a large number of possible values for x1 and x2
# and fill a list with the four possible distributions
grid = prepareWaferGrid(dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray)
trueFunction1 = gaussianDensity(axes = grid, mu = mu, sigma = sigma1)$pdf
trueFunction2 = parabolicDensity(axes = grid, coefficient = 1, ray = ray)$pdf
trueFunction3 = multiGaussianDensity(axes = grid, parameterList = parameterList)$pdf
trueFunction4 = uniformDensity(axes = grid)$pdf
distributionsList = list(gaussian = trueFunction1, 
                         parabolic = trueFunction2, 
                         multiGaussian = trueFunction3, 
                         uniform = trueFunction4)

for(i in 1:length(distributionsList)){
  distributionsList[[i]] = bindCircularMap(rectangularMap = distributionsList[[i]], dieWidth = dieWidth, dieHeight = dieHeight,  waferRay = ray, outValue = 0)
}
