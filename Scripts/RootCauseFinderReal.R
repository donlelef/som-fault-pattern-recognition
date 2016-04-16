# Find the equipment wich is most likely to be responsible of 
# a particular pattern of faults
library(lubridate)

# params
selectedCluster = 5
threshold = 100

# Import
source(file = "Scripts/Initializer.R")

# Load data
# dataFrame = data.frame(read.csv2(file = "Data/STM_Defect.csv"), stringsAsFactors = FALSE)
historyFrame = read.csv2(file = "Data/History.csv", as.is = TRUE)
historyFrame = unique(historyFrame[, c("LOT", "EQUIPMENT", "OPER", "QTY_OUT", "EVENT_TIME"), drop = FALSE] )
colnames(historyFrame) = c("LOT", "EQUIPMENT", "OPERATION", "QUANTITY", "TIME")
historyFrame$TIME = parse_date_time(historyFrame$TIME, orders = c("m!/d!/Y H!:M!"))

classificationFrame = read.csv2(file = "Data/Classification.csv", as.is = TRUE)

splittedIdsList = strsplit(classificationFrame$ID, "/", fixed = TRUE)
classificationLots = vector(mode = "character")
for (idSplitted in splittedIdsList) {
  classificationLots = c(classificationLots, idSplitted[1])
}
classificationFrame$ID = classificationLots

# Divide good and bad wafers
badWafers = classificationFrame[classificationFrame$FAULTS >= threshold, ]

# Select data for one cluster
clusterClassificationFrame = classificationFrame[classificationFrame$CLUSTER == selectedCluster, ]
clusterHistory = historyFrame[historyFrame$LOT %in% unique(clusterClassificationFrame$ID), ]
clusterHistory = clusterHistory[order(clusterHistory$TIME), ]
clusterBadWafers = badWafers[badWafers$ID %in% clusterClassificationFrame$ID & badWafers$CLUSTER == selectedCluster, ]

for (operation in clusterHistory$OPERATION) {
  opHist = clusterHistory[clusterHistory$OPERATION == operation, ]
  
  for (equipment in unique(opHist$EQUIPMENT)) {
    thisEqWafers = opHist[opHist$EQUIPMENT == equipment, ]
    otherEqWafers = opHist[opHist$EQUIPMENT != equipment, ]
    
    # Compute contingency table
    badEquipment = nrow(badWafers[badWafers$ID %in% thisEqWafers$LOT, ])
    goodEquipment = sum(unique.data.frame(thisEqWafers[, 1:4])$QUANTITY) - badEquipment
    badOther = nrow(badWafers[badWafers$ID %in% otherEqWafers$LOT, ])
    goodOther = sum(unique.data.frame(otherEqWafers[, 1:4])$QUANTITY) - badOther
    
    contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)
    
  }
}

