# Input fields reactive loading.

lots = reactive({
  lots = unique(as.character(dataFrameLoader()$LOT))
  lots = lots[order(lots, decreasing = FALSE)]
  return(lots)
})

wafers = reactive({
  wafers = unique(as.integer(getLotData(dataFrame = dataFrameLoader(), lot = input$lot)$WAFER))
  wafers = wafers[order(wafers, decreasing = FALSE)]
  return(wafers)
})

layers = reactive({
  layers = unique(as.integer(getWaferData(dataFrame = dataFrameLoader(), lot = input$lot, wafer = input$wafer)$LAYER))
  layers = layers[order(layers, decreasing = FALSE)]
  return(layers)
})

observe({
  updateSelectInput(session, "lot", choices = lots())
})

observe({
  updateSelectInput(session, "wafer", choices = wafers())
})

observe({
  updateSelectInput(session, "layer", choices = layers())
})