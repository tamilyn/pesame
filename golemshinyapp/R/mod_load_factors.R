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
    fileInput(ns("file"), 'Sample Data File'),
    uiOutput(ns("fileContentsPreview"))
  )
}
    
#' load_factors Server Function
#'
#' @noRd 

mod_load_factors_server <- function(id, factorFileData) {
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
  
   userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)

      fl <- factorFileData()
      fl$filename <- input$file$name
      factorFileData(fl)
      input$file
    })

  # output$fileContentsPreview ----
  output$fileContentsPreview <- renderUI({
    fd <- factorFileData()
    s <- fd$metadata
    if(is.null(s)) {
      return(p("No data loaded."))
    }
    numRows <- ifelse(is.null(s), "", nrow(s))
    numCols <- ifelse(is.null(s), "", ncol(s))
    msg <- glue::glue("ROWS {numRows} COLS {numCols}")
    dims <- glue::glue("{numRows} x {numCols}")
    tagList(
       div(
         span(style="font-weight: 900;", "File "),
         span(style="font-color: #660099;", fd$filename),
         span(style="font-weight: 900;", " Dimensions "),
         span(style="font-color: #660099;", dims)),
      div(span(style="font-weight: 900;", "Transpose? "),
           fd$transpose),
       div("Has ID column? ", fd$hasIdCol),
       actionButton(ns("adjusterBtn"), "Preview") 
    )

   })
   
    setmetadata <- function(df) { 
      if(is.null(df)) {
        print("setmetadata DF IS NULL")
        return
      }

      tryCatch({
          fl <- factorFileData()
          fl$metadata <- df
          fl$orig_metadata <- df

          # reset everything to empty (in case of error)
          fl$computedDetails <- NULL
          factorFileData(fl)

          factors_df <- identifyFactors(df)
          rdetails = factors_df %>% dplyr::filter(ready)

          fl <- factorFileData()
          fl$allOriginalFactor <- factors_df

          #TO DO: maybe remove first one if fl$hasIdCol 
          fl$availableFactors <- rdetails$name
          fl$computedDetails <- factors_df
          factorFileData(fl)

      }, warning = function(w) {
          flog.warn(str_c("identify factors warning ", w))
      }, error = function(e) {
          flog.error(str_c("identify factors warning ", e))
          browser()
      })
    }

    loadedTransformedData <- reactive({
      s <- factorFileData()$metadata
      transp <- input$transpose
      if(!is.null(s) ) {
        if(transp) {
           s <- t(s)
        }
      }
      s
    })

  output$tablePreview <- DT::renderDataTable({
    s <- loadedTransformedData()
    if(is.null(s)) {
      print("No data to preview")
      return(NULL)
    }

    if(nrow(s) > 3) {
      s1 <- s[1:3, ] %>% as.data.frame()
      if(ncol(s1) > 3) {
         s1 <- s1[, 1:3]
      }
      return(DT::datatable(s1))
    }
    DT::datatable(s)
  })


    observeEvent(input$file, {
       # look at file type and figure out what it is and save it
       ff <- userFile()
       
       dp <- userFile()$datapath
       for(i in seq_along(dp)) { 
            entry <- dp[[i]]
            the_ext <- tools::file_ext(entry)

           if(the_ext == "csv") {

             d = data.table::fread(entry)
             df1 <- as.data.frame(d)

             fd <- factorFileData()
             fd$rawFileData <- df1
             factorFileData(fd)

             if(fd$hasIdCol) {
               df2 <- select(df1, -1)
               setmetadata(df2) 
             } else {
               setmetadata(df1) 
             }

          } else {
            print("Not csv, now try to load the Excel")
          }
       } 
   })
    

    observeEvent(input$transpose, {
      fl <- factorFileData() ; fl$transpose <- input$transpose ; factorFileData(fl)
    })

    observeEvent(input$hasIdCol , {
      fl <- factorFileData()  
      fl$hasIdCol <- input$hasIdCol  

      df1 <- fl$rawFileData
      if(input$hasIdCol) {
        md_df <- select(df1, -1)
        setmetadata(md_df) 
      } else {
        setmetadata(df1) 
      }

      factorFileData(fl)
    })

  observeEvent(input$adjusterBtn, { 
    showModal(modalDialog(
        title = "Preview and Adjust",
        span(style="color:blue; font-size:120%;",
        checkboxInput(ns("hasIdCol"),
             "First column is instance name", input$hasIdCol)),
        checkboxInput(ns("transpose"), "Transpose", input$transpose),
        DT::dataTableOutput(ns("tablePreview")),
        size = "l",
        easyClose = TRUE
      ))
  })
  })

}
    
## To be copied in the UI
# mod_load_factors_ui("load_factors_ui_1")
    
## To be copied in the server
# callModule(mod_load_factors_server, "load_factors_ui_1")
 
