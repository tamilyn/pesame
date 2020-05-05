#' outernav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_outernav_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' outernav Server Function
#'
#' @noRd 
mod_outernav_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_outernav_ui("outernav_ui_1")
    
## To be copied in the server
# callModule(mod_outernav_server, "outernav_ui_1")
 
