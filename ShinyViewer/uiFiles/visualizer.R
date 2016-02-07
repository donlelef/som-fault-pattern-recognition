# UI for wafer visualizer

fluidPage(
  
  tags$head(
    tags$title("Wafer visualizer"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css")
  ),
  
  
  fluidRow(
    column(2, img(src = "Images/statwolf2.png")),
    column(8, offset = 2, titlePanel(title = "Wafer visualizer"))
  ),
  
  fluidRow(
    column(4, offset = 8, 
           conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                            tags$div(class = "alert alert-info fixed-on-top",
                                     tags$h5(id = "loadingTitle", 
                                             tags$i(class = "fa fa-spinner fa-pulse"), 
                                             "PERFORMING..."),
                                     tags$p("Sorry Sir, this will take a while. Please be patient!")
                            )
           )
    )
  ), 
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("START"),
      helpText("Start the computation. It may take some minutes. Be sure you selected a file in Data manager tab."),
      actionButton(inputId = "processDataFrame", label = "Process", class = "btn btn-success", icon = icon(name = "play", lib = "font-awesome")),
      
      hr(),
      
      h4("WAFER SELECTION"),
      selectInput(inputId = "lot", label = "Lot:", choices = ""),
      selectInput(inputId = "wafer", label = "Wafer:", choices = ""),
      selectInput(inputId = "layer", label = "Layer:", choices = ""),
      
      hr(),
      
      h4("PLOT OPTIONS"),
      
      selectInput(inputId = "girdSize", label = "Choose grid size: ", 
                  choices = list("2x2" = 2, "3x3" = 3, "4x4" = 4),
                  selected = 3),
      
      selectInput(inputId = "topology", label = "Choose grid topology: ", 
                  choices = list("Rectangular" = "rectangular", "Hexagonal" = "hexagonal")),
      
      sliderInput(inputId = "iterations", label = "Choose number of iterations: ",
                  min = 10, max = 1000, value = 100, step = 10),
      
      radioButtons(inputId = "somPlot", label = "Choose the SOM plot:", 
                   choices = list("Wafer" = "Wafer", "Count" = "count", "Training" = "changes"), 
                   selected = "Wafer"),
      
      
      
      hr(),
      
      h4("EXPORT"),
      helpText("Download a csv file with 3 fields for each wafer: an ID, the number of faults and the cluster."),
      downloadButton("downloadClassification", label = "Download Classification", class = "btn btn-primary")
      
      
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("waferMap")),
        column(6, plotOutput("waferKDE"))
      ),
      fluidRow(
        column(6, plotOutput("waferSOM")),
        column(6, plotOutput("waferClassification"))
      )
    )
  )
  
)