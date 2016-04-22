# Find the equipment wich is most likely to be responsible of 
# a particular pattern of faults

# params
selectedClusters = 1
threshold = 0

# Import
source(file = "Scripts/Initializer.R")

# Load data
historyFrame = read.csv2(file = "Data/History.csv", as.is = TRUE)
historyFrame = getHistoryData(dataFrame = historyFrame)
classificationFrame = read.csv2(file = "Data/Classification.csv", as.is = TRUE)

# Select data for one cluster
res = rootCauseAnalysis(classificationFrame = classificationFrame, historyFrame = historyFrame, threshold = threshold, clustrers = selectedClusters)

# Plot data
plotData = unique.data.frame(res[,2:3])[1:10, ]
plotData$EQUIPMENT = factor(plotData$EQUIPMENT, levels = rev(plotData$EQUIPMENT))
rootCauseBarPlot(x = plotData$EQUIPMENT, y = plotData$VALUE, title = "Top Chi Squared Values", xlab = "EQUIPMENT", ylab = "VALUE")

# view data frame
# View(res)
# write.csv2(x = res, file = "Data/RootCauseResult.csv", row.names = FALSE)
all.equal(read.csv2("Data/RootCauseResult.csv", as.is = TRUE), res)

