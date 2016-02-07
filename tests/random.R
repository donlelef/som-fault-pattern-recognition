# Cose a caso

data = read.csv2(file = "Data/Temporary Data/LOTTO3.csv")
layer = getLayerData(dataFrame = data, lot = "LOT3", wafer = 4, layer = 1420)

orX = mean(layer$DIST_X - layer$DIE_DIST_X - layer$ALIGNMENT_X - layer$DIE_WIDTH*layer$DIE_X)
orY = mean(layer$DIST_Y - layer$DIE_DIST_Y - layer$ALIGNMENT_Y + layer$DIE_HEIGHT*layer$DIE_Y)