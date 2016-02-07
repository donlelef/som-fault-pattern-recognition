# UI for wafer visualizer

fluidPage(
  
  tags$head(
    tags$title("Data manager"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css")
  ),
  
  
  fluidRow(
    column(2, img(src = "Images/statwolf2.png")),
    column(4, offset = 2, titlePanel(title = "Data manager"))
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
      h4("LOADING"),
      helpText("Select files to load. No more than 100 MB. Old browsers - including IE 9 and earlier - do not support multiple choice."),
      fileInput("csvInputFile", "Choose CSV (.csv) file(s):", accept = c(".csv"), multiple = TRUE),
      selectInput(inputId = "csvSeparator", label = "Choose separator for csv files: ", 
                  choices = list(";" = ";", "," = ",")),
      selectInput(inputId = "csvDecimal", label = "Choose decimal separator for csv files: ", 
                  choices = list("," = ",", "." = ".")),
      hr(),
      
      h4("EXPORT"),
      helpText("Merge all the current file and export in csv format"),
      downloadButton("downloadMerged", label = "Download data", class = "btn btn-primary")
    ),
    mainPanel(
      tableOutput("selectedFileTable"),
      dataTableOutput("selectedFileData")
    )
  )  
)