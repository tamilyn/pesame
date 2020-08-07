#' factors UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_factors_ui <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("selectDropdown")),
    uiOutput(ns("factorTables"))
  )
}
    
#' factors Server Function
#'
#' @noRd 
#' @import kableExtra
#' @import skimr
#' @import dplyr
mod_factors_server <- function(id, factorFileData) {

   moduleServer( id, function(input, output, session) { 

     ns <- session$ns

  ### allFactorDetails ----
  allFactorDetails <- reactive({
    fl <- factorFileData()
    if(!is.null(fl)) {
       left_join(fl$computedDetails, skimr::skim(fl$orig_metadata), 
           by=c("name"="skim_variable")) 
    }
  })

  ### output$availableFactorsTable ----
  output$availableFactorsTable <- DT::renderDataTable({
    dd <- allFactorDetails() %>% filter(ready)
    if(!"numeric.hist" %in% colnames(dd)) {
      dat <- dd %>%
        select(name, labels, method_applied, type, description,
          true_label, n_missing)
    } else {
      dat <- dd %>%
        select(name, labels, method_applied, type, description,
         true_label, n_missing, numeric.hist)
    }
    datatable(dat)
 })



 ### output$notAvailableFactorsTable ----
 output$notAvailableFactorsTable <- DT::renderDataTable({
   
   details2 <- allFactorDetails() %>%
     filter(!ready) %>%
     select(ready, name, labels, num_unique, everything())

   details2
 })

  ## output$factorTables ----
  output$factorTables = renderUI({

    num_avail <- allFactorDetails() %>% filter(ready) %>% nrow()
    num_unavail <- allFactorDetails() %>% filter(!ready) %>% nrow()

    if(num_avail > 0) {
       atable <- DT::dataTableOutput(ns("availableFactorsTable"))
    } else {
      atable <- p("No available factors")
    }

    if(num_unavail > 0) {
       natable <- DT::dataTableOutput(ns("notAvailableFactorsTable"))
    } else {
      natable <- p("All factors useable")
    }
    
    #tagList(h2("Non factor Variables"), natable, h2("Factors"), atable)
    tagList(h2("Factors"), atable)
  })



  ## output$factorVariables ----
  output$factorVariables = renderUI({
    af <- availableFactors()
    if(length(af) > 0) {
      selectInput('selectedFactor', 'Select factor', af )
    } else {
      h1("No available factors")
    }
  })

   selectedFactorLabels <- reactive({
     req(input$selectedFactor)

    fl <- factorFileData()
    this_factor <- fl$computedDetails %>%
            filter(name == input$selectedFactor)
    this_factor$labels
  })

  ## output$selectDropdown ----
   output$selectDropdown <- renderUI({
    fl <- factorFileData()
    af <- fl$computedDetails
    if(!is.null(af)) {
       #print(head(af))
       af <- af %>% dplyr::pull(name)
       tagList(
         selectInput(ns('selected'), 'Factor', af ),
         actionButton(ns("adjusterBtn"), "Edit Factor")
       )
    } else {
      p("no available factors")
    }
   })


 selectedFactorValues <- reactive({
   factorFileData()$orig_metadata %>% pull(input$selected) %>% unique
 })

 selectedKind <- reactive({ 

   validate( need(input$selected,
                  "No factors to show because no meta data loaded"))

    selected <- input$selected
    details <- factorFileData()$allOriginalFactor
    kind <- details %>%
            dplyr::filter(name == input$selected) %>%
            dplyr::pull(type)
    kind
 })

 output$selectedFactorDescription <- renderUI({
   if(is.null(input$selected)) {
     return(NULL)
   }

   selected <- input$selected
   af <- factorFileData()$computedDetails

   this_factor <- af %>% filter(name == selected)
   if(is.null(this_factor)) {
      print(glue::glue("164: no this_factor Selected: {selected}"))
      return(NULL)
   }
   #print(glue::glue("192: this_factor {this_factor} Selected: {selected}"))
   if(is.null(this_factor) || this_factor$type == "numeric") {
      return(NULL)
   }

   fvals <- str_split(this_factor$unique_values, ",") %>% unlist

   # based on type of factor
   tagList(
     textInput(ns("level0Label"), "Level 0 Label", fvals[1]),
     textInput(ns("level1Label"), "Level 1 Label", fvals[2]),
     div(actionButton(ns("applyFactorGroupButton"),"Factor Apply Levels")))
 })


  setDetails <- function(details) {
       rdetails = details %>% dplyr::filter(ready)
       fl <- factorFileData()
       fl$allOriginalFactor <- details
       fl$availableFactors <- rdetails$name
       factorFileData(fl)
  }


  ### applyFactorNumeric
  applyFactorNumeric <- function(ft, fmethod) {
    orig_md <- factorFileData()$orig_metadata
    md <- factorFileData()$metadata

    selected_factor <- orig_md %>% pull(ft)
    if (fmethod == "median") {
       midpoint = median(selected_factor, na.rm = TRUE)
       new_labels = "below median,above median"
       new_true = "above median"
    } else {
       midpoint = mean(selected_factor, na.rm = TRUE)
       new_labels = "below mean,above mean"
       new_true = "above mean"
    }
    md[ , ft ] <- orig_md[ , ft] > midpoint

    fl <- factorFileData()
    fl$metadata <- md
    factorFileData(fl)

    details <- factorFileData()$allOriginalFactor

    mp <- round(midpoint,2)
    new_descript <- str_c("0 = below or at ", fmethod,
        " 1 = above (", mp, ")",sep = "")

    n_df <- tibble(name=ft,
                   new_ready=TRUE,
                   new_method_applied=fmethod,
                   des=new_descript)

    k_df <- details %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
            mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
            mutate(description = ifelse(is.na(des), description, des)) %>%
            mutate(method_applied =
              ifelse(is.na(new_method_applied),
                    method_applied, new_method_applied)) %>%
            mutate(labels =
              ifelse(is.na(new_method_applied),
                    labels, new_labels)) %>%
            mutate(true_label =
              ifelse(is.na(new_method_applied),
                    true_label, new_true)) %>%
            select(-new_ready, -des, -new_method_applied)

    details <- z_df

    cd <- factorFileData()$computedDetails %>%
            mutate(ready, ifelse(name==ft,TRUE,ready)) %>%
            mutate(method_applied, ifelse(name==ft,fmethod,method_applied))  %>%
            mutate(description, ifelse(name==ft,new_descript,description))


    k_df <- factorFileData()$computedDetails %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
      mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
      mutate(description = ifelse(is.na(des), description, des)) %>%
      mutate(method_applied =
               ifelse(is.na(new_method_applied),
                      method_applied, new_method_applied)) %>%

            mutate(labels =
              ifelse(is.na(new_method_applied),
                    labels, new_labels)) %>%
            mutate(true_label =
              ifelse(is.na(new_method_applied),
                    true_label, new_true)) %>%
      select(-new_ready, -des, -new_method_applied)

    cd <- z_df

    fl <- factorFileData(); fl$computedDetails <- cd; factorFileData(fl)

    setDetails(details)
  }


  ### applyFactorGroup
  applyFactorGroup <- function(ft, level0) {
    flog.info(str_c("applyFactorGroup: [", ft, "] level0 ", 
       glue::glue_collapse(level0, sep = ", ")))

    fl <- factorFileData()
    orig_md <- fl$orig_metadata
    md <- fl$metadata

    selected_factor <- orig_md %>% pull(ft)
    new_factor <- map_lgl(selected_factor, ~ !(. %in% level0))

    md[ , ft ] <- new_factor
    fl$metadata <- md
    factorFileData(fl)

    details <- factorFileData()$allOriginalFactor

    new_descript <- str_c("Factor group applied")

    tp <- "factorgroup"
    f_labels <- str_c(input$level0Label, ",", input$level1Label, sep="")
    f_true_label = input$level1Label
    other_factors <- details %>% dplyr::filter(name != ft)

    # get the labels and true values
    this_factor <- details %>%
       dplyr::filter(name == ft) %>%
       mutate(ready = TRUE,
              method_applied = "factorgroup",
              description = "Factor group applied",
              labels = f_labels,
              true_label = f_true_label)

    details <- bind_rows(this_factor, other_factors)

    fl <- factorFileData()
    cd_other <- fl$computedDetails %>% dplyr::filter(name != ft)
    cd_this <- fl$computedDetails %>% dplyr::filter(name == ft)
    cd_this <- cd_this %>%
       mutate(ready = TRUE,
              method_applied = "factorgroup",
              description = "Factor group applied",
              labels = f_labels,
              true_label = f_true_label)
    cd <- bind_rows(cd_other, cd_this)

    flog.info("UPDATING applyFactorGroup computedDetails")

    fl$computedDetails <- cd ; factorFileData(fl)
    setDetails(details)
  }


  observeEvent(input$adjusterBtn, {
    kind <- selectedKind()
    #based on kind show a different popup
    if(kind=="numeric") {
      showModal(modalDialog(
          title = glue("Factor {input$selected} - (numeric)"),
          span(style="color:blue; font-size:150%;",input$selected),
          actionButton(ns("applyMean"), "Mean"),
          actionButton(ns("applyMedian"), "Median"),
          size = "l",
          easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
          title = glue("CHARACTER {input$selected}"),
          span(style="color:blue; font-size:150%;",input$selected),
# need to preselect the current value
          checkboxGroupInput(ns("level0checkboxes"),
             "Which value should be level 0?", selectedFactorValues()),
          uiOutput(ns("selectedFactorDescription")),
          size = "l",
          easyClose = TRUE
       ))
    }
  })


  ## BUTTON to apply changes ----
  observeEvent(input$applyMean, {
     applyFactorNumeric(input$selected, "mean")
     removeModal()
  })

  observeEvent(input$applyMedian, {
     applyFactorNumeric(input$selected, "median")
    removeModal()
  })

  observeEvent(input$applyFactorGroupButton, {
    b <- glue::glue_collapse(input$level0checkboxes, sep = ", ")
    applyFactorGroup(input$selected, input$level0checkboxes) 
    removeModal()
  })
  })
}
    
## To be copied in the UI
# mod_factors_ui("factors_ui_1")
    
## To be copied in the server
# callModule(mod_factors_server, "factors_ui_1")
 
