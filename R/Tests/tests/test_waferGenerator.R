test.waferGenerator = function(){
  
  # Define execution parameters
  waferPerDistribution = c(5, 5, 5, 5)
  minFaults = 20
  maxFaults = 60
  
  # Initialization
  source(file = "Script/initializer.R")
  waferIDCounter = 1
  faultY = faultX =vector(mode = "numeric")
  wafers = vector(mode = "character")
  
  for (j in 1:length(distributionsList)) {
    
    for (i in 1:waferPerDistribution[j]) {
      # Fill a simulated wafer with good and bad chips according to the just computed density.
      faults = floor(runif(n = 1, min = minFaults, max = maxFaults))
      faultMap = bindDefectNumber(probabilityMatrix = distributionsList[[j]], 
                                  faultValue = 1, notFaultValue = 0, 
                                  faultNumber = faults
      )
      # Find fault positions on the map and save them
      thisFaultPositions = findFaultPositions(faultMap = faultMap, dieWidth = dieWidth, dieHeight = dieHeight, faultValue = 1)
      thisWaferID = paste("wafer", waferIDCounter, sep = " ")
      
      faultX = c(faultX, thisFaultPositions[ ,1])
      faultY = c(faultY, thisFaultPositions[ ,2])
      wafers = c(wafers, rep(x = thisWaferID, times = nrow(thisFaultPositions)))
      waferIDCounter = waferIDCounter + 1
      
      # TEST
      print(faults)
      print(thisFaultPositions[ ,1])
      checkEquals(target = faults, current = length(thisFaultPositions[ ,1]))
      checkEquals(target = faults, current = length(thisFaultPositions[ ,2]))      
    }
  }
  
  # TEST
  checkEquals(target = length(faultX), current = length(wafers))
  
  # Saving the data
  data = data.frame(wafer = wafers, faultX = faultX, faultY = faultY)
  saveRDS(data, "simulatedWafers.rds", ascii=TRUE)
  
  # Loading data
  dataFrame = readRDS(file = "simulatedWafers.rds")

  # TEST
  checkEquals(target = length(dataFrame$wafer), current = length(wafers))
  
}


