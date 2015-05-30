test.generateWaferFromDistribution = function(){
  
  # Define execution parameters
  source(file = "Script/initializer.R")
  waferPerDistribution = c(5, 5, 5, 5)
  minFaults = 20
  maxFaults = 60
  
  data = generateWaferFormDistribution(distributionsList = distributionsList, waferPerDistribution = waferPerDistribution, defectsRange = c(minFaults, maxFaults),dieWidth = dieWidth, dieHeight = dieHeight)
  
  checkEquals(target = 3, current = ncol(data))
  checkTrue(expr = (nrow(data) > minFaults * sum(waferPerDistribution)) && (nrow(data) < maxFaults * sum(waferPerDistribution)))
  checkEquals(target = sum(waferPerDistribution), current = length(unique(data$wafer)))
  
}


