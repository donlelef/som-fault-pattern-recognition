library(shiny)
source("ShinyInitializer.R")
source("serverFiles/options.R")

shinyServer(function(input, output, session) {
  
  source("serverFiles/reactiveComputing.R", local = TRUE)
  source("serverFiles/inputUpdating.R", local = TRUE)
  source("serverFiles/outputComputing.R", local = TRUE)
  source("serverFiles/fileManagement.R", local = TRUE)
  
})
