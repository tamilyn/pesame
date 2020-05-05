#' load_factors UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_factors_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="loadmodule",
      fluidRow(column(6,
       span(class="filelabel", textOutput(ns("displayFilename")))),
      column(6,
       span(class="filelabel", textOutput(ns("numRows"))))),
      fileInput(ns("file"), 'Metadata (factors) File') 
    )
  )
}
    
#' load_factors Server Function
#'
#' @noRd 
mod_load_factors_server <- function(input, output, session, 
  metadataFilename, orig_metadata, metadata, computedDetails,
  allOriginalFactor, availableFactors) {

  ns <- session$ns

  loadmodule_data <- reactiveVal(
     list(dataFilename = NULL, df = NULL))
  
   userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)
      metadataFilename(input$file$name)
      input$file
    })

    output$numRows <- renderText({
      s <- orig_metadata()
      numRows <- ifelse(is.null(s), "", nrow(s))
      numRows
    })

    output$displayFilename <- renderText({
      metadataFilename()
    })

    readcsv <- function(dp) {
      d = data.table::fread(dp)
      as.data.frame(d)
    }

    setDetails <- function(details) {
       rdetails = details %>% dplyr::filter(ready)
       allOriginalFactor(details)
       availableFactors(rdetails$name)
    }
   
    setmetadata <- function(df,fn) { 
      if(is.null(df)) {
        print("setmetadata DF IS NULL")
        return
      }

      metadata(df)
      orig_metadata(df)

      # reset everything to empty
      computedDetails(NULL)

      tryCatch({
          factors_df <- identify_factors(df)
          setDetails(factors_df)
          computedDetails(factors_df)
      }, warning = function(w) {
          flog.warn(str_c("identify factors warning ", w))
      }, error = function(e) {
          flog.error(str_c("identify factors warning ", e))
          browser()
      })
    }

    observeEvent(input$file, {
       # look at file type and figure out what it is and save it

       ff <- userFile()
       fn <- metadataFilename()
       dp <- userFile()$datapath
       #print(glue::glue("OBSERVE input$file FILENAME {metadataFilename()}"))

       for(i in seq_along(dp)) { 
            entry <- dp[[i]]
            the_ext <- tools::file_ext(entry)
            the_fn <- fn[[i]]

           if(the_ext == "csv") {
             df1 <- readcsv(entry)
             ###loadedRawFileData(df1)
             df <- select(df1, -1)
             ###loadedFileData(df)

             #print(glue::glue("loaded file {the_fn} {nrow(df)} {ncol(df)} ext {the_ext}"))
             #print(head(df,3))
             the_list <- list(num_rows = nrow(df), num_cols = ncol(df), ext = the_ext,
                                     filename = the_fn, df = df)

             setmetadata(df, the_fn) 
          } else {
            print("Not csv, now try to load the Excel")
          }
       } 
   })
    
   return(loadmodule_data)
}
    
## To be copied in the UI
# mod_load_factors_ui("load_factors_ui_1")
    
## To be copied in the server
# callModule(mod_load_factors_server, "load_factors_ui_1")
 
