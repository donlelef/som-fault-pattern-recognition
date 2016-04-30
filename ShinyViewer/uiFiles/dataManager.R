# UI for data files management

fluidPage(
  
  tags$head(tags$title("Data manager")),
  
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
      h4("LOAD DATA"),
      helpText("Select files to load. No more than 100 MB. Old browsers - including IE 9 and earlier - do not support multiple choice."),
      fileInput("csvDefectFile", "Choose CSV (.csv) file(s) containing defect data:", accept = c(".csv"), multiple = TRUE),
      fileInput("csvHistoryFile", "Choose CSV (.csv) file(s) containing history data:", accept = c(".csv"), multiple = TRUE),
      selectInput(inputId = "csvSeparator", label = "Choose separator for csv files: ", 
                  choices = list(";" = ";", "," = ",")),
      selectInput(inputId = "csvDecimal", label = "Choose decimal separator for csv files: ", 
                  choices = list("," = ",", "." = "."))
    ),
    
    mainPanel(
      conditionalPanel("output.fileDataPresent", tags$h4("Selected Files", class = "center-text")), 
      DT::dataTableOutput("fileData"),
      conditionalPanel("output.defectSummaryPresent", tags$h4("Defect Data Summary", class = "center-text space-before")),
      DT::dataTableOutput("defectFileSummary"),
      conditionalPanel("output.historySummaryPresent", tags$h4("History Data Summary", class = "center-text space-before")),
      DT::dataTableOutput("historyFileSummary")
    )
  )
)
