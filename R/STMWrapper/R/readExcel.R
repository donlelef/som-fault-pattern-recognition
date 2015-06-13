#' This function loads an Excel file which is assumed to contain a data frame
#' and, optionally, saves it on a file.
#' 
#' @title Read an Excel file
#' @export
#' @param xlsToRead: the path of the Excel file to be read.
#' @param saveOnRDS: a logical flag. If set to TRUE, the returned data frame
#' is saved to a file, too. Default is FALSE.
#' @param rdsToWrite: if "saveOnRDS" is TRUE, the path of the rds file where the data 
#' should be saved.
#' If nothing is specified, a defaut file "xlsData.rds" is used.
#' @param ...: argument to be passed to lower functions.
#' @return a data.frame containing the data in the Excel file.
#' @import gdata

readExcel = function(xlsToRead, saveOnRDS = TRUE, rdsToWrite = "xlsData.rds", ...){
  
  library(gdata)
  
  data = read.xls(xls = xlsToRead, ...)
  if(saveOnRDS){
    saveRDS(object = data, file = rdsToWrite, ascii = TRUE) 
  }
  return(data)
}
  