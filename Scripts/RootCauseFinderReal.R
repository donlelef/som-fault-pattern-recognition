# Find the equipment wich is most likely to be responsible of 
# a particular pattern of faults
library(lubridate)
library(ggplot2)
library(ggthemes)

# params
selectedCluster = 4
threshold = 100

# Import
source(file = "Scripts/Initializer.R")

# Load data
# dataFrame = data.frame(read.csv2(file = "Data/STM_Defect.csv"), stringsAsFactors = FALSE)
historyFrame = read.csv2(file = "Data/History.csv", as.is = TRUE)
historyFrame = unique(historyFrame[, c("LOT", "EQUIPMENT", "OPER", "QTY_OUT", "EVENT_TIME"), drop = FALSE] )
historyFrame = historyFrame[complete.cases(historyFrame), ]
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

res = data.frame()
for (operation in unique(clusterHistory$OPERATION)) {
  opHist = clusterHistory[clusterHistory$OPERATION == operation, ]
  
  for (equipment in unique(opHist$EQUIPMENT)) {
    thisEqWafers = opHist[opHist$EQUIPMENT == equipment, ]
    otherEqWafers = opHist[opHist$EQUIPMENT != equipment, ]
    
    if(nrow(otherEqWafers) > 0){
      badEquipment = nrow(clusterBadWafers[clusterBadWafers$ID %in% thisEqWafers$LOT, ])
      goodEquipment = sum(unique.data.frame(thisEqWafers[, 1:4])$QUANTITY) - badEquipment
      badOther = nrow(clusterBadWafers[clusterBadWafers$ID %in% otherEqWafers$LOT, ])
      goodOther = sum(unique.data.frame(otherEqWafers[, 1:4])$QUANTITY) - badOther
      
      if(badEquipment / (goodEquipment + badEquipment) > badOther / (goodOther + badOther)){
        contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)
        testValue = chisq.test(x = contingencyMatrix, correct = FALSE)
        res = rbind(res, 
                    data.frame(OPERATION = operation, 
                               EQUIPMENT = equipment, 
                               VALUE = testValue$statistic, 
                               BAD_EQUIPMENT = badEquipment,
                               GOOD_EQUIPMENT = goodEquipment,
                               BAD_OTHER = badOther,
                               GOOD_OTHER = goodOther,
                               P_VALUE = testValue$p.value,
                               stringsAsFactors = FALSE))  
      }
    }
  }
}
res = res[order(res$VALUE, decreasing = TRUE), ]

# Plot data
plotData = unique.data.frame(res[,2:3])[1:10, ]
plotData$EQUIPMENT = factor(plotData$EQUIPMENT, levels = rev(plotData$EQUIPMENT))
ggplot(data = plotData, mapping = aes(x = EQUIPMENT, y = VALUE)) +
  geom_bar(mapping = aes(fill = VALUE), stat = "identity", position = position_dodge(), show.legend = FALSE) +
  coord_flip(ylim = c(min(plotData$VALUE)*0.9, max(plotData$VALUE)*1.1)) +
  ggtitle("Top 10 Chi squared values") +
  theme_classic(base_family = "serif") + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.5, linetype = "dotted"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.ontop = TRUE) +
  scale_fill_gradient(low = "#9999ff", high = "#000066", guide = FALSE)
  
# view data frame
View(res)



