#' load_data_content_side  - load data side panel
#'
#' @import bsplus shinyjs
load_data_content_side <- div("Select data and meta data source")

#' citation_side - citation_side panel
#'
citation_side <-
   div("")


#' citation_main - citation main panel
#'
citation_main <-
  div(textOutput("citation"))

#' top_part - top navigation bar
#'
top_part <- tags$nav(class="navbar navbar-default navbar-fixed-top",
  div(class="container",
    tags$ul(class="nav navbar",
      tags$li(class="navbar-text navbar-brand", "PESAME: Predictive effect size analysis in multivariable ensembles"))))


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @export
#' @noRd
app_ui <- function(request) {
  version <- paste0("v", packageVersion("golemshinyapp"))
  #print(version)

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here 
    fluidPage( 
      useShinyjs(), # TAMI CHECK

      tags$head(
        tags$title('PESAME'),
        tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')),
      top_part,
       div(
    bs_accordion_sidebar(id = "sections",
                         spec_side = c(width = 2, offset = 0),
                         spec_main = c(width = 10, offset = 0)) %>%
      bs_append( title_side = "Load data",
                 content_side = load_data_content_side,
                 content_main = tagList(
                   fluidRow(
                     column(6, mod_loadmodule_ui("loadmodule_ui_1")),
                     column(6, mod_load_factors_ui("loadmodule_ui_2"))),
                   fluidRow(mod_preview_table_ui("preview_table_ui_1"))
                   )) %>%
      bs_append( title_side = "Factors",
                 content_side = "Select factors",
                 content_main = mod_factors_ui("factors_ui_1")) %>%
      bs_append( title_side = "Analyze",
                 content_side = "View graphs",
                 content_main = mod_analyze_ui("analyze_ui_1")) %>%
      bs_append( title_side = "Citation",
                 content_side = citation_side,
                 content_main = citation_main),

    use_bs_tooltip(),
    use_bs_popover(),
    use_bs_accordion_sidebar())
#, tags$pre(tags$code(paste0("Version: ", version)))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'golemshinyapp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    , shinyalert::useShinyalert() 
  )
}

