library(shiny)

shinyUI(
  navbarPage(
    "Wafer fault tool",
    tabPanel("Data manager",
             source(file = "uiFiles/dataManager.R", local = TRUE, print.eval = FALSE)[["value"]]
    ),
    tabPanel("Visualization",
             source(file = "uiFiles/visualizer.R", local = TRUE, verbose = FALSE)[["value"]]
    )
  )
)