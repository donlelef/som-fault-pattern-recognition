# This script generates an arbitrary number of wafer with a random number of defects. 
# The amount of faults can range between given limits. The number of generated wafer 
# is arbitrary, too.

# Imports
library(SOMWaferClassification)

# Initialize distributions
source(file = "Script//initializer.R")

# Define execution parameters
waferPerDistribution = c(5, 5, 5, 5)
minFaults = 20
maxFaults = 60

# Generate Wafer
data = generateWaferFormDistribution(distributionsList = distributionsList, 
                                     waferPerDistribution = waferPerDistribution, 
                                     defectsRange = c(minFaults, maxFaults), 
                                     dieWidth = dieWidth, 
                                     dieHeight = dieHeight)

# Saving the data
saveRDS(data, "simulatedWafers.rds", ascii=TRUE)
