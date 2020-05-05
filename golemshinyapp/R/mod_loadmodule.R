#' loadmodule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import glue
mod_loadmodule_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="loadmodule",
      fluidRow(column(6,
       span(class="filelabel", textOutput(ns("displayFilename")))),
      column(6,
       span(class="filelabel", textOutput(ns("numRows"))))),
      fileInput(ns("file"), 'Otut File') 
    )
  )
}
    
#' loadmodule Server Function
#'
#' @import glue
#' @noRd 
mod_loadmodule_server <- function(input, output, session, 
  title = 'Hiya', hasfactors, loadedFileData, orig_metadata, metadata, computedDetails, mvdata) {
  ns <- session$ns
  title <- title

   loadmodule_data <- reactiveVal(
     list(thedescription = "Hey You Description", 
          dataFilename = NULL,
          df = NULL))
  
   loadedFilename <- reactiveVal(NULL)
   loadedFileData <- reactiveVal(NULL)
   loadedRawFileData <- reactiveVal(NULL)

   metadataFilename <- reactiveVal(NULL)
   metadata <- reactiveVal(NULL)
   orig_metadata <- reactiveVal(NULL)


   userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)
      loadedFilename(input$file$name)
      input$file
    })

    output$numRows <- renderText({
      s <- loadedFileData()
      numRows <- ifelse(is.null(s), "", nrow(s))
      numRows
    })

    output$displayFilename <- renderText({
      s <- loadedFilename()
      s
    })

    readcsv <- function(dp) {
      d = data.table::fread(dp)
      as.data.frame(d)
    }
   
    setmetadata <- function(df,fn) { 
      #df <- loadedFileData()
      #fn <- loadedFilename()

      if(is.null(df)) {
        print("DF IS NULL")
        return
      }
      print(str_c("metadata filename: ", fn, " #rows ", nrow(df), " ", ncol(df)))

      metadataFilename(fn)
      metadata(df)
      orig_metadata(df)
    }

    observeEvent(input$file, {
       # look at file type and figure out what it is and save it

       print(glue::glue("OBSERVE input$file FILENAME {loadedFilename()}"))
       ff <- userFile()
       fn <- loadedFilename()
       dp <- userFile()$datapath

       for(i in seq_along(dp)) { 
            entry <- dp[[i]]
            the_ext <- tools::file_ext(entry)
            the_fn <- fn[[i]]

           if(the_ext == "csv") {
             df1 <- readcsv(entry)
             loadedRawFileData(df1)
             df <- select(df1, -1)
             loadedFileData(df)

             #print(glue::glue("loaded file {the_fn} {nrow(df)} {ncol(df)} ext {the_ext}"))
             #print(glue::glue("set mvdata: ", glue::glue_collapse(dim(df), sep = " x " )))
               mvdata(df)
          } else {
            print("Not csv, now try to load the Excel")
          }
       } 
   })
    
   return(loadmodule_data)
}
    
## To be copied in the UI
# mod_loadmodule_ui("loadmodule_ui_1")
    
## To be copied in the server
# callModule(mod_loadmodule_server, "loadmodule_ui_1")
 
