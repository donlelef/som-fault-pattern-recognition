# This script loads and sava STM data.

library(STMWrapper)
data = readExcel(xlsToRead = "STMData/LOTTO1_DEF.xlsx", saveOnRDS = TRUE, rdsToWrite = "lotto1.rds")
