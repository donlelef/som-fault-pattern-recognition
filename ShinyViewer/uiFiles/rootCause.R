# UI for root cause analysis

fluidPage(
  
  tags$head(
    tags$title("Wafer visualizer"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css")
  ),
  
  
  fluidRow(
    column(2, img(src = "Images/statwolf2.png")),
    column(8, offset = 2, titlePanel(title = "Root cause detection"))
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
      helpText("Start the computation. It may take some minutes. Be sure you selected a wafer data file and an operation history file."),
      actionButton(inputId = "processRootCause", label = "Process", class = "btn btn-success", icon = icon(name = "play", lib = "font-awesome")),
      
      hr(),
      
      h4("CLUSTER SELECTION"),
      selectizeInput(inputId = "clusters", label = "Clusters:", choices = "", multiple = TRUE),
      
      hr(),
      
      h4("OPTIONS"),
      sliderInput(inputId = "threshold", label = "Choose max faults for good wafer: ",
                  min = 0, max = 1000, value = 100, step = 10),
      selectizeInput(inputId = "equipments", label = "Choose equipments to be excluded from the analysis: ", 
                  choices = "", multiple = TRUE),
      
      hr(),
      
      h4("EXPORT"),
      helpText("Download a csv file with full analysis output"),
      downloadButton("downloadRootCause", label = "Download analysis results", class = "btn btn-primary")
      
    ),
    
    mainPanel(
      plotOutput("rootCauseBarPlot"),
      dataTableOutput("rootCauseData")
    )
  )
  
)