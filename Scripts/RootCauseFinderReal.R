# Find the equipment wich is most likely to be responsible of 
# a particular pattern of faults

createContingencyMatrix = function(thisEqWafers, otherEqWafers, badWafers) {
  
  badEquipment = nrow(badWafers[badWafers$LOT %in% thisEqWafers$LOT, ])
  goodEquipment = sum(unique.data.frame(thisEqWafers[, 1:4])$QUANTITY) - badEquipment
  badOther = nrow(badWafers[badWafers$LOT %in% otherEqWafers$LOT, ])
  goodOther = sum(unique.data.frame(otherEqWafers[, 1:4])$QUANTITY) - badOther
  
  contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)
  colnames(contingencyMatrix) = c("Bad", "Good")
  rownames(contingencyMatrix) = c("Equipment", "Others")
  
  return(contingencyMatrix)
}

checkContingencyMatrix = function(matrix){
  eqWafer = sum(matrix[ ,1])
  otherWafer = sum(matrix[ ,2])
  eqYield = matrix[2,1] / eqWafer
  otherYield =  matrix[2,2] / otherWafer
  isValid = (eqWafer != 0) && (otherWafer != 0) && eqYield < otherYield
  return(isValid)
}

testEquipment = function(contingencyMatrix){
  testValue = chisq.test(x = contingencyMatrix, correct = FALSE)
  res = data.frame(VALUE = testValue$statistic,
                   BAD_EQUIPMENT = contingencyMatrix[1,1],
                   GOOD_EQUIPMENT = contingencyMatrix[2,1],
                   BAD_OTHER = contingencyMatrix[1,2],
                   GOOD_OTHER = contingencyMatrix[2,2],
                   P_VALUE = testValue$p.value,
                   stringsAsFactors = FALSE)
  return(res)
}

rootCauseDetection = function(classificationFrame, historyFrame, badWafers) {
  res = data.frame()
  
  for (operation in unique(historyFrame$OPERATION)) {
    opHist = historyFrame[historyFrame$OPERATION == operation, ]
    for (equipment in unique(opHist$EQUIPMENT)) {
      thisEqWafers = opHist[opHist$EQUIPMENT == equipment, ]
      otherEqWafers = opHist[opHist$EQUIPMENT != equipment, ]
      contingencyMatrix = createContingencyMatrix(thisEqWafers = thisEqWafers, otherEqWafers = otherEqWafers, badWafers = badWafers)
      if(checkContingencyMatrix(contingencyMatrix)){
        testValue = testEquipment(contingencyMatrix)
        res = rbind(res, data.frame(OPERATION = operation,
                                    EQUIPMENT = equipment, 
                                    testValue,
                                    stringsAsFactors = FALSE))  
      }
    }
  }
  res = res[order(res$VALUE, decreasing = TRUE), ]
}

rootCauseAnalysis = function(classificationFrame, historyFrame, threshold, clustrers){
  
  clustrers = as.numeric(clustrers)
  
  classificationFrame = classificationFrame[classificationFrame$CLUSTER %in% clustrers, ]
  badWafers = classificationFrame[classificationFrame$FAULTS >= threshold, ]
  historyFrame = historyFrame[historyFrame$LOT %in% unique(classificationFrame$LOT), ]
  historyFrame = historyFrame[order(historyFrame$TIME), ]
  res = rootCauseDetection(classificationFrame = classificationFrame, historyFrame = historyFrame, badWafers = badWafers)
  
  return(res)
}

plotRootCauses = function(equipments, values, title){
  plotData = data.frame(EQUIPMENT = equipments, VALUE = values)
  plotData$EQUIPMENT = factor(plotData$EQUIPMENT, levels = rev(plotData$EQUIPMENT))
  ggplot(data = plotData, mapping = aes(x = EQUIPMENT, y = VALUE)) +
    geom_bar(mapping = aes(fill = VALUE), stat = "identity", position = position_dodge(), show.legend = FALSE) +
    coord_flip(ylim = c(min(plotData$VALUE)*0.9, max(plotData$VALUE)*1.1)) +
    ggtitle(title) +
    theme_classic(base_family = "serif") + 
    theme(panel.grid.major.x = element_line(colour = "black", size = 0.5, linetype = "dotted"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.ontop = TRUE) +
    scale_fill_gradient(low = "#9999ff", high = "#000066", guide = FALSE)
}

# params
selectedClusters = 1
threshold = 0

# Import
source(file = "Scripts/Initializer.R")

# Load data
# dataFrame = data.frame(read.csv2(file = "Data/STM_Defect.csv"), stringsAsFactors = FALSE)
historyFrame = read.csv2(file = "Data/History.csv", as.is = TRUE)
historyFrame = getHistoryData(dataFrame = historyFrame)
classificationFrame = read.csv2(file = "Data/Classification.csv", as.is = TRUE)

# Select data for one cluster
res = rootCauseAnalysis(classificationFrame = classificationFrame, historyFrame = historyFrame, threshold = threshold, clustrers = selectedClusters)

# Plot data
plotData = unique.data.frame(res[,2:3])[1:10, ]
plotRootCauses(equipments = plotData$EQUIPMENT, values = plotData$VALUE, title = "Top Chi Squared Values")

# view data frame
# View(res)
# write.csv2(x = res, file = "Data/RootCauseResult.csv", row.names = FALSE)
all.equal(read.csv2("Data/RootCauseResult.csv", as.is = TRUE), res)

