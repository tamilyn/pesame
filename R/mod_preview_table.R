#' preview_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preview_table_ui <- function(id){
  ns <- NS(id)
  tagList(
   tabsetPanel(type = "tabs",
     tabPanel("Features", tableOutput(ns("previewOtutTable"))),
     tabPanel("Sample Data", tableOutput(ns("previewMetadataTable")))))
}
    
#' preview_table Server Function
#'
#' @noRd 
mod_preview_table_server <- function(id, filesData, factorFileData) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$previewOtutTable <- renderTable({
      filesData()$mvdata
    })
    
    output$previewMetadataTable <- renderTable({
      factorFileData()$metadata
    })
    
  })
}

## To be copied in the UI
# mod_preview_table_ui("preview_table_ui_1")
    
## To be copied in the server
# callModule(mod_preview_table_server, "preview_table_ui_1")
 
