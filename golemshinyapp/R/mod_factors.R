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
    uiOutput(ns("factorSelectionArea")),
    uiOutput(ns("editFactorUI")),
    tableOutput(ns("factorTable"))
  )
}
    
#' factors Server Function
#'
#' @noRd 
#' @import kableExtra
#' @import skimr
#' @import dplyr
mod_factors_server <- function(id, factorFileData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    availableFactors <- reactive({
      fl <- factorFileData()
      fl$factorDetails %>% dplyr::filter(ready) %>% pull(name)
    })
    
    ## output$factorVariables ----
    output$factorVariables = renderUI({
      af <- availableFactors()
      if (length(af) > 0) {
        selectInput('selectedFactor', 'Select factor', af)
      } else {
        h1("No available factors")
      }
    })
    
    selectedFactorLabels <- reactive({
      req(input$selectedFactor)
      
      fl <- factorFileData()
      this_factor <- fl$factorDetails %>%
        filter(name == input$selectedFactor)
      this_factor$labels
    })
    
    ## output$factorSelectionArea ----
    output$factorSelectionArea <- renderUI({
      fl <- factorFileData()
      af <- fl$factorDetails
      if (!is.null(af)) {
        #print(head(af))
        af <- af %>% dplyr::pull(name)
        tagList(fluidRow(column(6, selectInput(
          ns('selected'), 'Factor', af
        )),
        column(6, uiOutput(
          ns("selectedFactorOverview")
        ))))
      } else {
        p("no available factors")
      }
    })
    
    
    selectedFactorValues <- reactive({
      factorFileData()$orig_metadata %>% pull(input$selected) %>% unique
    })
    
    selectedKind <- reactive({
      validate(need(
        input$selected,
        "No factors to show because no meta data loaded"
      ))
      
      selected <- input$selected
      details <- factorFileData()$factorDetails
      kind <- details %>%
        dplyr::filter(name == input$selected) %>%
        dplyr::pull(type)
      kind
    })
    
    output$selectedFactorDescription <- renderUI({
      if (is.null(input$selected)) {
        return(NULL)
      }
      
      selected <- input$selected
      af <- factorFileData()$factorDetails
      
      this_factor <- af %>% filter(name == selected)
      if (is.null(this_factor)) {
        print(glue::glue("164: no this_factor Selected: {selected}"))
        return(NULL)
      }
      
      if (is.null(this_factor)
          || this_factor$type == "numeric"
          || this_factor$type == "integer") {
        return(NULL)
      }
      
      # only shown for character factor
      tagList(
        textInput(
          ns("level0Label"),
          "Level 0 Label",
          this_factor$false_label
        ),
        textInput(
          ns("level1Label"),
          "Level 1 Label",
          this_factor$true_label
        ),
        div(actionButton(
          ns("assignLevelsBtn"), "Assign Values to Levels"
        ))
      )
    })
    
    output$selectedFactorOverview <- renderUI({
      if (is.null(input$selected)) {
        return(NULL)
      }
      
      selected <- input$selected
      af <- factorFileData()$factorDetails
      
      this_factor <- af %>% filter(name == selected)
      if (is.null(this_factor)) {
        print(glue::glue("164: no this_factor Selected: {selected}"))
        return(NULL)
      }
      
      fvals <- str_split(this_factor$unique_values, ",") %>% unlist
      num_vals <- length(fvals)
      
      bg <- ifelse(this_factor$ready, "lightgreen" , "pink")
      styling <-
        glue::glue(
          "color:blue; background-color: {bg}; padding: 5px; border: thin solid navy; font-size:120%;"
        )
      method <- ifelse(
        this_factor$method_applied == "none",
        "",
        glue("Method: {this_factor$method_applied}")
      )
      
      uvals <- ifelse(
        this_factor$type == "character",
        glue("Values: {this_factor$unique_values}"),
        "numeric"
      )
      
      msg <- ifelse(

        this_factor$ready,
        "This factor is ready and available to use in the analysis.  
        
        This variable has levels which can be assigned to 0 (FALSE) or 1 (TRUE)",
        ifelse( this_factor$type == "character",
                "This variable must be processed before it can be analyzed.
        
        This variable can be partitioned by assigning each value to one of two categories",
                
        "This variable must be processed before it can be analyzed.
        
        This variable can be partitioned by using the mean or median"
      ))
      
      tagList(div(style = styling, p(msg)))
    })
    
    ### applyFactorNumeric
    applyFactorNumeric <- function(ft, fmethod) {
      orig_md <- factorFileData()$orig_metadata
      md <- factorFileData()$metadata
      
      #print(glue::glue("166 APPLY {ft} method {fmethod}"))
      selected_factor <- orig_md %>% pull(ft)
      
      if (fmethod == "median") {
        midpoint = median(selected_factor, na.rm = TRUE)
        new_labels = "below median,above median"
        new_true = "above median"
        new_false = "below median"
      } else {
        midpoint = mean(selected_factor, na.rm = TRUE)
        new_labels = "below mean,above mean"
        new_true = "above mean"
        new_false = "below mean"
      }
      md[, ft] <- orig_md[, ft] > midpoint
      
      fl <- factorFileData()
      fl$metadata <- md
      factorFileData(fl)
      
      fd <- fl$factorDetails
      otherfactor <- filter(fd, name != ft)
      
      mp <- round(midpoint, 2)
      new_descript <- glue::glue("0 <= {fmethod}, 1 = above ({mp})")
      
      updated <- fd %>%
        filter(name == ft) %>%
        mutate(
          method_applied = fmethod,
          labels = new_labels,
          true_label = new_true,
          false_label = new_false,
          ready = TRUE,
          description = new_descript
        )
      
      newone <- bind_rows(otherfactor, updated)
      fl$factorDetails <- newone
      factorFileData(fl)
    }
    
    
    ### applyLevels
    applyLevels <- function(ft, level0) {
      b <- glue::glue_collapse(level0, sep = ",")
      flog.info(str_c("applyLevels: [", ft, "] level0 ==> ",  b))
      
      fl <- factorFileData()
      orig_md <- fl$orig_metadata
      md <- fl$metadata
      
      selected_factor <- orig_md %>% pull(ft)
      new_factor <- map_lgl(selected_factor, ~ !(. %in% level0))
      
      md[, ft] <- new_factor
      fl$metadata <- md
      factorFileData(fl)
      
      details <- factorFileData()$factorDetails
      
      f_labels <-
        str_c(input$level0Label, ",", input$level1Label, sep = "")
      f_true_label = input$level1Label
      f_false_label = input$level0Label
      other_factors <- details %>% dplyr::filter(name != ft)
      
      # get the labels and true values
      this_factor <- details %>%
        dplyr::filter(name == ft) %>%
        mutate(
          ready = TRUE,
          method_applied = "factorgroup",
          description = "Factor group applied",
          level0_values = b,
          labels = f_labels,
          false_label = f_false_label,
          true_label = f_true_label
        )
      
      details <- bind_rows(this_factor, other_factors)
      
      #flog.info("UPDATING applyLevels factorDetails")
      fl$factorDetails <- details
      factorFileData(fl)
    }
    
    output$editFactorUI <- renderUI({
      kind <- selectedKind()
      #based on kind show a different popup
      
      if (kind == "numeric" || kind == "integer") {
        tagList(
          span(style = "color:blue; font-size:120%;", input$selected),
          actionButton(ns("applyMean"), "Mean"),
          actionButton(ns("applyMedian"), "Median")
        )
      } else {
        fl <- factorFileData()
        item <- fl$factorDetails %>% filter(name == input$selected)
        initial_level0 <-
          str_split(item$level0_values,
                    pattern = ",",
                    simplify = TRUE)
        
        tagList(
          span(style = "color:blue; font-size:120%;", input$selected),
          
          # need to preselect the current value
          checkboxGroupInput(
            ns("level0checkboxes"),
            "Which value(s) should be 0 (FALSE)? Other values will be 1 (TRUE)",
            selectedFactorValues(),
            inline = TRUE,
            selected = initial_level0,
            width = '100%'
          ),
          uiOutput(ns("selectedFactorDescription"))
        )
      }
    })
    
    observeEvent(input$adjusterBtn, {
      kind <- selectedKind()
      #based on kind show a different popup
      
      if (kind == "numeric" || kind == "integer") {
        showModal(
          modalDialog(
            title = glue("Factor {input$selected} - (numeric)"),
            span(style = "color:blue; font-size:120%;", input$selected),
            actionButton(ns("applyMean"), "Mean"),
            actionButton(ns("applyMedian"), "Median"),
            size = "l",
            easyClose = TRUE
          )
        )
      } else {
        fl <- factorFileData()
        item <- fl$factorDetails %>% filter(name == input$selected)
        initial_level0 <-
          str_split(item$level0_values,
                    pattern = ",",
                    simplify = TRUE)
        
        showModal(
          modalDialog(
            title = glue("CHARACTER {input$selected}"),
            span(style = "color:blue; font-size:120%;", input$selected),
            
            # need to preselect the current value
            checkboxGroupInput(
              ns("level0checkboxes"),
              "Which value(s) should be 0 (FALSE)? Other values will be 1 (TRUE)",
              selectedFactorValues(),
              inline = TRUE,
              selected = initial_level0,
              width = '100%'
            ),
            uiOutput(ns("selectedFactorDescription")),
            size = "l",
            easyClose = TRUE
          )
        )
      }
    })
    
    
    output$factorTable <- renderTable({
      fl <- factorFileData()
      if(!is.null(fl$factorDetails)) {
        fl$factorDetails %>% 
          select(name, ready, type, method_applied,
                false_label, true_label, level0_values)
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
    
    observeEvent(input$assignLevelsBtn, {
      applyLevels(input$selected, input$level0checkboxes)
      removeModal()
    })
  })
}
    
## To be copied in the UI
# mod_factors_ui("factors_ui_1")
    
## To be copied in the server
# callModule(mod_factors_server, "factors_ui_1")
 
