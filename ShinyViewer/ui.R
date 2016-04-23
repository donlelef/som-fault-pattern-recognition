library(shiny)

shinyUI(
  navbarPage(
    "Wafer fault tool",
    tabPanel("Data manager",
             source(file = "uiFiles/dataManager.R", local = TRUE)[["value"]]
    ),
    tabPanel("Fault visualisation",
             source(file = "uiFiles/faultVisualiser.R", local = TRUE)[["value"]]
    ), 
    tabPanel("Root cause detection",
             source(file = "uiFiles/rootCause.R", local = TRUE)[["value"]]
    )
  )
)