# Reactive data loading & computing for server.R

defectFrameTriggered = eventReactive(input$processDefect, {
  if(!is.null(defectData())){
    defectData()
  }
})

rootCauseFrameTriggered = eventReactive(input$processRootCause, {
  if(!is.null(rootCauseResultFrame())){
    rootCauseResultFrame()
  }
})

KDEFrameLoader = reactive({
  if(!is.null(defectFrameTriggered())){
    getKDEData(dataFrame = defectFrameTriggered())
  }
})

globalFeatureLoader = reactive({
  if(!is.null(defectFrameTriggered())){
    getWaferFeatures(dataFrame = defectFrameTriggered())
  }
})

globalGridLoader = reactive({
  if(!is.null(globalFeatureLoader())){
    features = globalFeatureLoader()
    createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
  }
})

distributionsLoader = reactive({
  if(!is.null(KDEFrameLoader()) && !is.null(globalFeatureLoader())){
    distributions = distributions(KDEdataFrame = KDEFrameLoader(), features = globalFeatureLoader())
    return(distributions)
  }  
})

somObjectLoader = reactive({
  if(!is.null(distributionsLoader())){
    set.seed(12)
    waferSom = som(data = distributionsLoader(), 
                   grid = somgrid(xdim = input$girdSize, ydim = input$girdSize, topo = input$topology), 
                   rlen = input$iterations)
    return(waferSom)
  }
})

classificationFrameLoader = reactive({
  if(!is.null(somObjectLoader())){
    classification = classificationFrame(trainedSomObject = somObjectLoader(), KDEFrame = KDEFrameLoader())
    return(classification)
  }
})

layerDataLoader = reactive({
  if(!(is.null(defectFrameTriggered()) || is.null(input$lot) || is.null(input$wafer) || is.null(input$layer))){
    layerData = getLayerData(dataFrame = defectFrameTriggered(), lot = input$lot, wafer = input$wafer, layer = input$layer)
    if(nrow(layerData) > 0){
      return(layerData)
    } else {
      return(NULL)
    }
  }
})

layerKDELoader = reactive({
  if(!is.null(layerDataLoader())){
    layerKDE = getKDEData(dataFrame = layerDataLoader())
  }
})

layerFeatureLoader = reactive({
  if(!is.null(layerDataLoader())){
    getWaferFeatures(dataFrame = layerDataLoader())
  }
})

layerClassificationLoader = reactive({
  if(!is.null(layerKDELoader())){
    distribution = distributions(KDEdataFrame = layerKDELoader(), features = globalFeatureLoader())
    thisWaferClassification = classifyWafers(distributions = distribution, trainedKohonenObject = somObjectLoader())
  }
})

waferMapLoader = reactive({
  if(!is.null(layerFeatureLoader())){
    features = layerFeatureLoader()
    grid = createSTMGrid(waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight)
    index = indexFromCoord(coords = layerKDELoader()[, 2:3], features = features, grid = grid)
    waferFaultMap(features = features, index = index, faultValue = 1, nonFaultValue = 0, outValue = NA)
  }
})

waferKDELoader = reactive({
  if(!is.null(layerFeatureLoader())){
    features = layerFeatureLoader()
    waferKDE = waferKDEMap(features = features, coords = layerKDELoader()[, 2:3])
    bindCircularMap(rectangularMap = waferKDE, waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight, outValue = NA)
  }
})

rootCauseResultFrame = reactive({
  if(!is.null(input$clusters) && !is.null(historyData()) && !is.null(classificationFrameLoader())){
    historyFrame = getHistoryData(dataFrame = historyData(), filterEquipments = input$equipments, filterOperations = input$operations)
    rootCauseOutput = rootCauseAnalysis(classificationFrameLoader(), historyFrame, input$threshold, as.numeric(input$clusters))
    rootCauseOutput = rootCauseOutput[,1:7]
    colnames(rootCauseOutput) = c("OPERATION", "EQUIPMENT", "VALUE", "BAD", "GOOD", "BAD_OTHER", "GOOD_OTHER")
    return(rootCauseOutput)
  }
})