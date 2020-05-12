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
    fluidRow(column(4, uiOutput(ns("selectDropdown"))),
             column(4, uiOutput(ns("selectFactorButtons"))),
             column(4, uiOutput(ns("selectedFactorDescription" )))),
    fluidRow(column(12, uiOutput(ns("factorTables"))))
  )
}
    
#' factors Server Function
#'
#' @noRd 
#' @import kableExtra
#' @import skimr
#' @import dplyr
mod_factors_server <- function(input, output, session, 
  orig_metadata, metadata, computedDetails, allOriginalFactor, availableFactors, mvdata){
  ns <- session$ns

  ### allFactorDetails ----
  allFactorDetails <- reactive({
    #orig_fd <- orig_metadata()
    #my_skim <- skimr::skim(orig_fd)

    computedDetails() %>% 
      left_join(skimr::skim(orig_metadata()), by=c("name"="skim_variable")) 
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

   #shiny::showNotification()
   #print(colnames(details2))
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
    
    tagList(h2("Non factor Variables"), natable, h2("Factors"), atable)
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

    this_factor <- computedDetails() %>%
            filter(name == input$selectedFactor)
    this_factor$labels
  })

  ## output$selectDropdown ----
   output$selectDropdown <- renderUI({
    af <- computedDetails()
    if(!is.null(af)) {
       #print(head(af))
       af <- af %>% dplyr::pull(name)
       selectInput(ns('selected'), 'Factor', af )
    } else {
      p("no available factors")
    }
   })


 selectedFactorValues <- reactive({
   orig_metadata() %>% pull(input$selected) %>% unique
 })

  ## output$selectFactorButtons ----
 output$selectFactorButtons <- renderUI({
   validate( need(input$selected,
                  "No factors to show because no meta data loaded"))

    selected <- input$selected

    details <- allOriginalFactor()
    kind <- details %>%
            dplyr::filter(name == input$selected) %>%
            dplyr::pull(type)

    uniq_vals <- orig_metadata() %>% dplyr::pull(selected) %>% unique

    if(kind=="numeric") {
     tagList(
        actionButton(ns("applyMean"), "Mean"),
        actionButton(ns("applyMedian"), "Median"))
    } else {
      tagList(
        #need to initialize with current settings
        checkboxGroupInput(ns("level0checkboxes"),
          "Which value should be level 0?", selectedFactorValues()))
    }
 })


 output$selectedFactorDescription <- renderUI({
   if(is.null(input$selected)) {
     return(NULL)
   }

   selected <- input$selected
   af <- computedDetails()

   this_factor <- af %>% filter(name == selected)
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
       allOriginalFactor(details)
       availableFactors(rdetails$name)
  }


  ### applyFactorNumeric
  applyFactorNumeric <- function(ft, fmethod) {
    orig_md <- orig_metadata()
    md <- metadata()

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
    metadata(md)

    details <- allOriginalFactor()

    mp <- round(midpoint,2)
    new_descript <- str_c("0 = below or at ", fmethod,
        " 1 = above (", mp, ")",sep = "")

    n_df <- data_frame(name=ft,
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

    cd <- computedDetails() %>%
            mutate(ready, ifelse(name==ft,TRUE,ready)) %>%
            mutate(method_applied, ifelse(name==ft,fmethod,method_applied))  %>%
            mutate(description, ifelse(name==ft,new_descript,description))


    k_df <- computedDetails() %>% left_join(n_df, by=c("name"="name"))

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

    computedDetails(cd)
    setDetails(details)
  }


  ### applyFactorGroup
  # BUG: updates the data but not the displayed table
  # BUG: does not update the labels
  applyFactorGroup <- function(ft, level0) {
    flog.info(str_c("applyFactorGroup: [", ft, "] level0 ", level0))

    orig_md <- orig_metadata()
    md <- metadata()

    selected_factor <- orig_md %>% pull(ft)
    new_factor <- map_lgl(selected_factor, ~ !(. %in% level0))

    md[ , ft ] <- new_factor
    metadata(md)

    details <- allOriginalFactor()

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

    cd_other <- computedDetails() %>% dplyr::filter(name != ft)
    cd_this <- computedDetails() %>% dplyr::filter(name == ft)
    cd_this <- cd_this %>%
       mutate(ready = TRUE,
              method_applied = "factorgroup",
              description = "Factor group applied",
              labels = f_labels,
              true_label = f_true_label)
    cd <- bind_rows(cd_other, cd_this)

    flog.info("UPDATING applyFactorGroup computedDetails")

    computedDetails(cd)
    setDetails(details)
  }


  observeEvent(input$applyMean, {
     applyFactorNumeric(input$selected, "mean")
  })

  observeEvent(input$applyMedian, {
     applyFactorNumeric(input$selected, "median")
  })

  observeEvent(input$applyFactorGroupButton, {
    b <- glue::glue_collapse(input$level0checkboxes, sep = ", ")
    applyFactorGroup(input$selected, input$level0checkboxes) 
  })
}
    
## To be copied in the UI
# mod_factors_ui("factors_ui_1")
    
## To be copied in the server
# callModule(mod_factors_server, "factors_ui_1")
 
