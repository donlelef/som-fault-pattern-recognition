# File manager for server.R

csvLoader = reactive({
  if(!is.null(input$csvInputFile)){
    data = data.frame()
    for(path in input$csvInputFile$datapath){
      data = rbind(data, read.csv2(file = path, stringsAsFactors = FALSE, sep = input$csvSeparator, dec = input$csvDecimal))
    }
    return(data)
  }
})

csvList = reactive({
  if(!is.null(input$csvInputFile)){
    dataList = list()
    for(i in 1:length(input$csvInputFile$datapath)){
      data = read.csv2(file = input$csvInputFile$datapath[i], sep = input$csvSeparator, dec = input$csvDecimal)
      dataList[[i]] = data
    }
    return(dataList)
  }
})

fileDataLoader = reactive({
  if(!is.null(csvList())){
    fileData = data.frame()
    for(dataFrame in csvList()){
      fileData = rbind(fileData, getFileSummary(dataFrame = dataFrame))
    }
    fileData = cbind(FILE = input$csvInputFile$name, fileData)
    return(fileData)
  }
})
