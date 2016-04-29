# Output for server.R

# Defect file summary
output$defectFileSummary = DT::renderDataTable({
  defectSummary()
}, extensions = c("Responsive"), options = list(searching = FALSE, 
                                                paging = FALSE, 
                                                info = FALSE, 
                                                processing = FALSE, 
                                                responsive = TRUE))

output$defectSummaryPresent = reactive({
  !is.null(defectSummary())
})
outputOptions(output, 'defectSummaryPresent', suspendWhenHidden = FALSE)

# History file summary
output$historyFileSummary = DT::renderDataTable({
  historySummary()
}, extensions = c("Responsive"), options = list(searching = FALSE, 
                                                paging = FALSE, 
                                                info = FALSE, 
                                                processing = FALSE, 
                                                responsive = TRUE))

output$historySummaryPresent = reactive({
  !is.null(historySummary())
})
outputOptions(output, 'historySummaryPresent', suspendWhenHidden=FALSE)


# Input files summary
output$fileData = DT::renderDataTable({
  if(!is.null(input$csvDefectFile) || !is.null(input$csvHistoryFile)){
    data = rbind(input$csvDefectFile, input$csvHistoryFile)
    data = data[, 1:3]
    data$size = data$size/1024
    names(data) = c("FILE", "SIZE (KB)", "TYPE")
    return(data)
  }
}, extensions = c("Responsive"), options = list(searching = FALSE, 
                                                paging = FALSE, 
                                                info = FALSE, 
                                                processing = FALSE, 
                                                responsive = TRUE))

output$fileDataPresent = reactive({
  !is.null(historySummary()) || !is.null(defectSummary())
})
outputOptions(output, 'fileDataPresent', suspendWhenHidden=FALSE)



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
  if(!(is.null(somObjectLoader()))){
    if(input$somPlot == "Wafer"){
      features = globalFeatureLoader()
      grid = globalGridLoader()
      kohonenCodesPlot(kohonenObject = somObjectLoader(), waferRay = features$waferRay, dieWidth = features$dieWidth, dieHeight = features$dieHeight, grid = grid)
    } else {
      plot(x = somObjectLoader(), type = input$somPlot, palette.name = rainbow)
    }
  }
})

# Download classification
output$downloadClassification = downloadHandler(
  filename = function() {
    paste(Sys.Date(), "Classification.csv", sep = "_")
  },
  content = function(file){
    write.csv2(x = classificationFrameLoader(), file = file)
  }
)

# Root cause output data
output$rootCauseData = DT::renderDataTable({
  rootCauseFrameTriggered()
}, extensions = c("Responsive"), options = list(searching = TRUE, 
                                                paging = TRUE, 
                                                info = TRUE, 
                                                responsive = TRUE,
                                                pageLength = 10, 
                                                dom = "<'row'<'col-sm-6'l><'col-sm-6'f>><'row'<'col-sm-12'tr>><'row'<'col-sm-4'i><'col-sm-8'p>>"))

# Root cause plot
output$rootCauseBarPlot = renderPlot({
  if(!is.null(rootCauseFrameTriggered())){
    plotFrame = rootCauseFrameTriggered()[1:10, ]
    rootCauseBarPlot(x = factor(plotFrame$EQUIPMENT, levels = rev(plotFrame$EQUIPMENT)), y = plotFrame$VALUE, title = "Chi square values for selected equipments", xlab = "EQUIPMENT", ylab = "VALUE")
  }
})

# Download classification
output$downloadRootCause = downloadHandler(
  filename = function() {
    paste(Sys.Date(), "RootCause.csv", sep = "_")
  },
  content = function(file){
    write.csv2(x = rootCauseFrameTriggered(), file = file)
  }
)