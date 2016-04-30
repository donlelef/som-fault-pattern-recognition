# Input fields reactive loading.

lots = reactive({
  lots = unique(as.character(defectData()$LOT))
  lots = lots[order(lots, decreasing = FALSE)]
  return(lots)
})

wafers = reactive({
  wafers = unique(as.integer(getLotData(dataFrame = defectData(), lot = input$lot)$WAFER))
  wafers = wafers[order(wafers, decreasing = FALSE)]
  return(wafers)
})

layers = reactive({
  layers = unique(as.integer(getWaferData(dataFrame = defectData(), lot = input$lot, wafer = input$wafer)$LAYER))
  layers = layers[order(layers, decreasing = FALSE)]
  return(layers)
})

clusters = reactive({
  clusters = unique(as.integer(classificationFrameLoader()$CLUSTER))
  clusters = clusters[order(clusters, decreasing = FALSE)]
})

equipments = reactive({
  equipments = unique(as.character(historyData()$EQUIPMENT))
  equipments = equipments[order(equipments, decreasing = FALSE)]
})

operations = reactive({
  operations = unique(as.character(historyData()$OPER))
  operations = operations[order(operations, decreasing = FALSE)]
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

observe({
  updateSelectInput(session, "clusters", choices = clusters())
})

observe({
  updateSelectInput(session, "equipments", choices = equipments())
})

observe({
  updateSelectInput(session, "operations", choices = operations())
})