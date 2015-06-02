test.KDEOnWafer = function(){
  
  # Initialize parameters
  source("Script//initializer.R")
  waferPerDistribution = c(2,2,2,2)
  
  waferData = generateWaferFormDistribution(distributionsList = distributionsList, 
                                waferPerDistribution = waferPerDistribution, 
                                defectsRange = c(60, 80), 
                                dieWidth = dieWidth,
                                dieHeight = dieHeight)
  
  waferMatrix = KDEOnWafer(dataFrame = waferData, dieWidth = dieWidth, dieHeight = dieHeight, waferRay = ray, plotDistributions = TRUE)
  
  checkEquals(target = sum(waferPerDistribution), current = nrow(waferMatrix))
  checkEquals(target = length(distributionsList[[1]]), current = ncol(waferMatrix))
  
}