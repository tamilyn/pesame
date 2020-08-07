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
      fileInput(ns("file"), 'Otut File'),
      uiOutput(ns("fileContentsPreview"))
    )
}
    
#' loadmodule Server Function
#'
#' @import glue
#' @noRd 
mod_loadmodule_server <- function(id, filesData) {

  moduleServer(id, function(input, output, session) {

   ns <- session$ns

   userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)

      fl <- filesData()
      fl[["filename"]] <- input$file$name
      filesData(fl)

      input$file
    })

    loadedTransformedData <- reactive({
      fd <- filesData()
      s <- fd$rawFileData
      if(!is.null(s) ) {
        if(fd$transpose) {
           s <- t(s)
        }
      }
      s
    })

  output$otutTablePreview <- DT::renderDataTable({
    s <- loadedTransformedData()
    if(!is.null(s) && nrow(s) > 3) {
      s1 <- s[1:3, ]
      if(ncol(s1) > 3) {
         s1 <- s1[, 1:3]
      }
      return(DT::datatable(s1))
    }
    DT::datatable(s)
  })

  
  # output$fileContentsPreview ----
  output$fileContentsPreview <- renderUI({
    fd <- filesData()
    s <- fd$rawFileData

    if(is.null(s)) {
      return(p("No data loaded."))
    }

    idCol <- !is.null(fd) && fd$hasIdCol
    numRows <- ifelse(is.null(s), "", nrow(s))
    numCols <- ifelse(is.null(s), "", ncol(s))
    filename <- fd$filename
    dims <- glue::glue("{numRows} x {numCols}")
    tagList(
       div(
         span(style="font-weight: 900;", "File "),
         span(style="font-color: #660099;", filename),
         span(style="font-weight: 900;", " Dimensions "),
         span(style="font-color: #660099;", dims)
         ),
       div(span(style="font-weight: 900;", "Transpose? "),
           fd$transpose),
       div("Has ID column? ", fd$hasIdCol),
       actionButton(ns("adjusterBtn"), "Preview") 
    )
  })

  observeEvent(input$adjusterBtn, { 
    fd <- filesData()
    showModal(modalDialog(
        title = "Preview and Adjust",
        span(style="color:blue; font-size:120%;",
        checkboxInput(ns("hasIdCol"), 
          "First column is id", fd$hasIdCol)),
        checkboxInput(ns("transpose"), "Transpose", fd$transpose),
        DT::dataTableOutput(ns("otutTablePreview")),
        size = "l",
        easyClose = TRUE
      ))
  })

    observeEvent(input$transpose, {
      fl <- filesData() ; 
      fl$transpose <- input$transpose  
      filesData(fl)
    })

    observeEvent(input$hasIdCol , {
      fl <- filesData()  
      fl$hasIdCol <- input$hasIdCol 
      fl$mvdata <- select(fl$rawFileData, -1)
      filesData(fl)
    })

    observeEvent(input$file, {
       # look at file type and figure out what it is and save it
       print(glue::glue("OBSERVE input$file FILENAME {filesData()$filename}"))
       ff <- userFile()
       fn <- filesData()$filename
       dp <- userFile()$datapath

       for(i in seq_along(dp)) { 
           entry <- dp[[i]]
           the_ext <- tools::file_ext(entry)
           the_fn <- fn[[i]]

           if(the_ext == "csv") {
             d = data.table::fread(entry)
             df1 <- as.data.frame(d)

             fl <- filesData()
             fl$rawFileData <- df1

             #if flag set, remove instance name
             fl$mvdata <- df1
             if(fl$hasIdCol) {
               fl$mvdata <- select(df1, -1)
             }
           
             filesData(fl)

          } else {
            print("Not csv, now try to load the Excel")
          }
       } 
   })
    
   })
}
    
## To be copied in the UI
# mod_loadmodule_ui("loadmodule_ui_1")
    
## To be copied in the server
# callModule(mod_loadmodule_server, "loadmodule_ui_1")
 
