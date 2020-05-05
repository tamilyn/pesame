#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny plotly
#' @import futile.logger 
#' @noRd
app_server <- function( input, output, session ) {
   # List the first level callModules here

    filesData <- reactiveVal(list())
    loadedFilename <- reactiveVal(NULL)
    loadedFileData <- reactiveVal(NULL)
    loadedRawFileData <- reactiveVal(NULL)
    metadataFilename <- reactiveVal(NULL)
    availableFactors <- reactiveVal(list())
    allOriginalFactor <- reactiveVal(list())
    computedDetails <- reactiveVal(NULL)
    metadata <- reactiveVal(NULL)
    mvdata <- reactiveVal(NULL)

   orig_metadata <- reactiveVal(NULL)

   dataModule <- callModule(mod_loadmodule_server, 
      "loadmodule_ui_1", "TITLE ONE", FALSE, 
       loadedFileData, orig_metadata, metadata, computedDetails, mvdata)

   metadataModule <- callModule(mod_load_factors_server, 
      "loadmodule_ui_2", metadataFilename, orig_metadata, 
      metadata, computedDetails, allOriginalFactor, availableFactors)

   factorsModule <- callModule(mod_factors_server, "factors_ui_1", 
       orig_metadata, metadata, computedDetails, allOriginalFactor, availableFactors, mvdata )

   analyzeModule <- callModule(mod_analyze_server, "analyze_ui_1", metadata, availableFactors, mvdata,
      computedDetails)

    output$citationUI <- renderUI({
        h1("citation panel")
    })
}
