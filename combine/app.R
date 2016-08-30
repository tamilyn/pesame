library(shiny)

fileFormats = c('text/csv','text/comma-separated-values', 'text/tab-separated-values', 'text/plain','.csv','.tsv')

trim <- function(x) gsub("^\\s+|\\s+$","",x)

ui <- fluidPage(
  titlePanel("Combine dataset and metadata into RData file"),
  sidebarLayout(
    sidebarPanel(
      withTags({ div(class="header", p("Specify a dataset file which contains a matrix."))}),
      fileInput('datasetFile', 'Data file', accept = fileFormats ),
      withTags({
        div(class="header",
            p("Specify a metadata file which contains a data.frame",
            "(rownames must make the colnames of the data file)")
        )}),
      fileInput('metadataFile', 'Metadata file', accept = fileFormats ), 
      withTags({
        div(class="header",
            p("The data file and metadata file will be combined into an RData file."),
            p("(provided that they are compatible - eg. rownames match columnnames)")
        )}),
      
      textInput("outputName", label = "Output Filename", value = "combined"),
      actionButton("updateButton", "Combine Files")
    ),

      mainPanel(
        tabsetPanel(type="tabs",
        tabPanel("Instructions", textOutput("instructions")),
        tabPanel("mvdata", dataTableOutput("mvdataDataTable")),
        tabPanel("metadata", dataTableOutput("metadataDataTable"))
    ))))

server <- function(input, output) {
  
  today = Sys.Date()
  now = date()
  print( paste( "shiny server started: ", now))
  
  # read csv or text
  metadata <- reactive( {
    
    if (is.null(input$metadataFile$datapath)) {
      return(NULL)
    }
    
    k <- read.csv2(input$metadataFile$datapath,header=TRUE)
    rownames(k) <- k[,1]
    k
  })
  
  mvdata <- reactive({
    
    if (is.null(input$datasetFile$datapath)) {
      return(NULL)
    }
    
    d <- read.csv2(input$datasetFile$datapath,header=TRUE)
    rownames(d) <- d[,1]
    d <- d[,-1] 
  })
  
  
  combineFiles <- function( outputName = "combined") {
    
    dd <- mvdata()
    mm <- metadata()
    
    if(is.null((dd)) || is.null(mm)) {
      print("missing one of the inputs")
      return(NULL)
    }
    
    if ( all(rownames(mm) == colnames(dd)) ) {
      oname <- paste(trim(outputName),".RData",sep="")
      print(paste("VALIDATED: colnames match row names, saving to ",oname))
      cc <- save(dd,mm,file=oname)
    } else {
      print(paste("combine FAILED: colnames do not match row names: ", 
                  rownames(mm), " data col = ", colnames(dd)))
      nc <- length(colnames(dd))
      nr <- length(rownames(mm))
      
      print(paste("there are ", nc, " cols and ", nr, " rows"))
    }
  }
  
  
  # When update button is pressed, combine the files
  observeEvent(input$updateButton, {
    if (is.null(input$outputName))
      return(NULL)

    combineFiles(  input$outputName )
  })
  

  output$mvdataDataTable <- renderDataTable({mvdata()})
  output$metadataDataTable <- renderDataTable({metadata()})
  
  output$instructions <- renderText({
          "Select a data set file, a metadata file, the name of the combined file, and press the combined button.  
    Once loaded, the dataset and metadata can be viewed using the tabs."
    })
}

shinyApp( ui = ui, server = server )
