# This script loads and sava STM data.

library(STMWrapper)
data = readExcel(xlsToRead = "STMData/LOTTO1_RIDOTTISSIMO.xlsx", saveOnRDS = TRUE, rdsToWrite = "lotto1.rds")
