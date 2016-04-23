# File manager for server.R
fileSummary = function(inputId, summaryFunc){
  
  fileSummaryLoader = reactive({
    if(!is.null(fileList(inputId)())){
      fileData = data.frame()
      for(dataFrame in fileList(inputId)()){
        fileData = rbind(fileData, summaryFunc(dataFrame = dataFrame))
      }
      fileData = cbind(FILE = input[[inputId]]$name, fileData)
      return(fileData)
    }
  })
  return(fileSummaryLoader)
  
}

fileList = function(inputId){
  
  fileListLoader = reactive({
    if(!is.null(input[[inputId]])){
      dataList = list()
      for(i in 1:length(input[[inputId]]$datapath)){
        data = read.csv2(file = input[[inputId]]$datapath[i], sep = input$csvSeparator, dec = input$csvDecimal, stringsAsFactors = FALSE)
        dataList[[i]] = data
      }
      return(dataList)
    }
  })
  return(fileListLoader)
}

fileData = function(inputId){
  
  fileDataLoader = reactive({
    if(!is.null(fileList(inputId)())){
      data = data.frame()
      for(dataFrame in fileList(inputId)()){
        data = rbind(data, dataFrame)
      }
      return(data)
    }
  })
  
  return(fileDataLoader)
  
}


defectSummary = fileSummary(inputId = "csvDefectFile", summaryFunc = getDefectFileSummary)
historySummary = fileSummary(inputId = "csvHistoryFile", summaryFunc = getHistoryFileSummary)
defectData = fileData(inputId = "csvDefectFile")
historyData = fileData(inputId = "csvHistoryFile")

