# Output for server.R

# Fault map plot
output$waferMap = renderPlot({
  if(!is.null(waferMapLoader())){
    matrixPlot(title = "Fault Distribution", matrix = waferMapLoader(), colorMap = c("blue", "red"))
  }
})

# Kde plot
output$waferKDE = renderPlot({
  if(!is.null(waferKDELoader())){
    matrixPlot(title = "Fault Probability Estimation", matrix = waferKDELoader(), colorMap = rev(brewer.pal(11, "RdYlBu")))
  }
})

# Classification plot
output$waferClassification = renderPlot({
  if(!is.null(layerClassificationLoader())){
    classificationPlot(kohonenObject = somObjectLoader(), matchingColor = "blue", nonMatchingColor = "white", cluster = layerClassificationLoader(), main = "Mapping")
  } 
})

# SOM plot
output$waferSOM = renderPlot({
  if(!(is.null(dataFrameLoader()))){
    if(input$somPlot == "Wafer"){
      features = globalFeatureLoader()
      grid = globalGridLoader()
      kohonenCodesPlot(kohonenObject = somObjectLoader(), waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight, grid = grid)
    } else {
      plot(x = somObjectLoader(), type = input$somPlot, palette.name = rainbow)
    }
  }
})

# File summary
output$selectedFileData = renderDataTable({
  fileDataLoader()
}, options = list(searching = FALSE, paging = FALSE))

# FileInput summary
output$selectedFileTable = renderTable({
  if(!is.null(input$csvInputFile)){
    data = input$csvInputFile[, 1:3]
    data$size = data$size/1024
    names(data) = c("file", "size (KB)", "type")
    return(data)
  }
})

# Download merged dataset
output$downloadMerged = downloadHandler(
  filename = function() {
    paste(Sys.Date(), "Data.csv", sep = "_")
  },
  content = function(file){
    write.csv2(x = csvLoader(), file = file)
  }
)

# Download classification
output$downloadClassification = downloadHandler(
  filename = function() {
    paste(Sys.Date(), "Classification.csv", sep = "_")
  },
  content = function(file){
    write.csv2(x = classificationFrameLoader(), file = file)
  }
)