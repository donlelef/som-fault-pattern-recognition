# This script performs KDE and SOM and then plots the cluster of each wafer.

# Import
source(file = "Scripts/Initializer.R")

# Load data
dataFrame = read.csv2(file = "Data/STM_Defect.csv")
KDEFrame = getKDEData(dataFrame = dataFrame)
features = getWaferFeatures(dataFrame = dataFrame)

# Iniatilize random seed and others parameters.
seed = 12 # for reproducibility
set.seed(seed)
palette = rev(brewer.pal(11, "RdYlBu"))
grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
ids = unique(as.character(KDEFrame$ID))

# Perform KDE SOM and classification
distributions = distributions(KDEdataFrame = KDEFrame, features = features)
waferSom = som(data = distributions, grid = somgrid(xdim = 3, ydim = 3, topo = "rectangular"), rlen = 100)
classification = classifyWafers(distributions = distributions, trainedKohonenObject = waferSom)

for(i in 1:length(ids)){
  
  thisWaferClassification = classification[i]
  splittedID = unlist(strsplit(x = ids[i], split = "[/]"))
  classificationPlot(kohonenObject = waferSom, matchingColor = "blue", nonMatchingColor = "white", cluster = thisWaferClassification, main = paste("Mapping", "Lot:", splittedID[1],  "Wafer:", splittedID[2], "Layer: ", splittedID[3], sep = " "))

}

classificationFrame = classificationFrame(trainedSomObject = waferSom, KDEFrame = KDEFrame)
# write.csv2(x = classificationFrame, file = "Data/Classification.csv", row.names = FALSE)
print(classificationFrame)
