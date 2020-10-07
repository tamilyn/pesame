#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny plotly
#' @import futile.logger 
#' @noRd
#' @export
app_server <- function( input, output, session ) {
   # List the first level callModules here

   filesData <- reactiveVal(list(transpose = FALSE, hasIdCol=FALSE))
   factorFileData <- reactiveVal(list(transpose = FALSE, hasIdCol=FALSE))

   mod_loadmodule_server("loadmodule_ui_1", filesData)
   mod_load_factors_server("loadmodule_ui_2", factorFileData)
   mod_preview_table_server("preview_table_ui_1", filesData, factorFileData)

   mod_factors_server("factors_ui_1", factorFileData)

   mod_analyze_server("analyze_ui_1", filesData, factorFileData)

    output$citationUI <- renderUI({
        h1("citation panel")
    })
}
