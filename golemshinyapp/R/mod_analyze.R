#' analyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analyze_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, p("Description")), 
      column(9, uiOutput(ns("description")))
    ),
    fluidRow(
      column(4, selectInput(ns("adj_method"),
		label = "Comparison Method", choices = method.options,
		selectize = FALSE,
		size = 1,
      selected = "fdr")),
      column(4, selectInput(inputId = ns("significance_threshold"),
		label= "Significance Threshold",
                selectize = FALSE,
		size = 1,
		choices = significance.options,
                selected = significance.options.default )) ,
      column(4,  uiOutput(ns("factorVariables")) )),
     uiOutput(ns("dataTabPanel"))
   )
}
    
#' analyze Server Function
#'
#' @import DT
#' @import Hmisc
#' @noRd 
mod_analyze_server <- function(id, filesData, factorFileData) {

  moduleServer(id, function(input, output, session) {
  ns <- session$ns

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    req(input$significance_threshold)

    sf <- theSelectedFactor()
    if(length(sf) == 0) {
      flog.info("98: baseFilteredData: no factor data")
      return(NULL)
    }

    otut <- otut_for_processing()
    if(is.null(otut)) {
      flog.info("102: baseFilteredData: no otut")
      return(NULL)
    }

    ft = tryCatch({
        suppressWarnings(helper.data_by_auc( otut = otut, fdata = sf, input$adj_method))
        }, warning = function(w) {
         flog.warn(str_c("110 baseFilteredData helper warning ", w))
         NULL
        }, error = function(w) {
         flog.error(str_c("113 baseFilteredData helper error ", w))
         NULL
        })

    if(is.null(ft)) {
      # need to display an error message
      flog.info("118: baseFilteredData NULL")
      return(NULL)
    }
    thres <- input$significance_threshold

    if(thres != "All") {
      thres <- as.numeric(input$significance_threshold)
      filt = (ft["p.adjust", ] < thres) & !is.na(ft["p.adjust", ])
      result <- t(ft[, filt, drop=F])
    } else {
      result <- t(ft)
    }
    result
  })

  ###########################################################
  # filteredData ----
  # Apply filter to base data
  # converting to data frame and augmenting
  filteredData <- reactive({
    req(baseFilteredData())
    b <- baseFilteredData()
    if (nrow(b) == 0) {
      flog.info("127: filteredData: no data")
      return(NULL)
    }

    dd = as.data.frame(b)
    ticks = generateTicks(dd)
    sf = theSelectedFactor()

    dd$Enriched = levels(sf)[(dd$auc < .5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })

   selectedFactorLabels <- reactive({
     req(input$selectedFactor)

     v <- filter(factorFileData()$computedDetails, name == input$selectedFactor)
     v$labels
  })


  ## output$factorVariables ----
  output$factorVariables = renderUI({
    af <- factorFileData()$availableFactors
    if(length(af) > 0) {
      selectInput(ns('selectedFactor'), 'Select factor', af )
    } else {
      h1("No available factors")
    }
  })

  roundedData <- reactive({
     fd <- filteredData() %>%
      mutate(auc = round(auc,5),
             se = round(se,5),
             low = round(low,5),
             high = round(high,5),
             p.value = round(p.value,5),
             p.adjust = round(p.adjust,5))
     fd
  })


  theSelectedFactor <- reactive({
    req(input$selectedFactor, factorFileData()$metadata)

    fd <- factorFileData()$metadata
    if(!input$selectedFactor %in% names(fd)) {
      flog.info("theSelectedFactor not available")
      #print(names(fd))
      #browser()
      return(NULL)
    }

    fd %>% pull(input$selectedFactor)
  })

  otut_for_processing <- reactive({

    tft <- filesData()$mvdata
    if(is.null(tft)) {
      print("no mvdata() ")
      return(NULL)
    }
    return(as.matrix(tft))
  })


  ############################################################
  # generateTicks ----
  # generates sequence for tick marks
  generateTicks <- function(dd) {
    tickMarkers = round(min(0.5, max(abs(c(dd$high, dd$low) - 0.5))), 2)
    seq(-tickMarkers, tickMarkers, length.out = 5)
  }


  getDD <- reactive({

    req(input$significance_threshold, input$selectedFactor)
    #print(glue::glue("GET DD factor: {input$selectedFactor} threshold: {input$significance_threshold}"))

    dd <- filteredData()
    if(is.null(dd) || length(dd) == 0) {
       flog.info("filteredPlotly: NO DATA")
       return(NULL)
    }

    sf = theSelectedFactor()

    dd$Enriched = levels(sf)[(dd$auc<.5) + 1]
    dd$heights = dd$auc - 0.5
    dd$auc1 = dd$heights
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })

  # use plot_ly to display data
  # generatePlot_ly ----
  generatePlot_ly <- reactive({ 
    dd <- getDD()
    if(length(dd) <= 0) {
      flog.error(paste("126: returning empty plot_ly,no rows"))
      return(ggplot())
    }

    ticks = generateTicks(dd)
    my.xaxis = list( title = "auc",
                     autotick = TRUE,
                     ticktextsrc = format(ticks+0.5, digits=3),
                     showticklabels = TRUE,
                     showline = TRUE,
                     linecolor = toRGB("gray"),
                     linewidth = 1,
                     ticktext = format(ticks+0.5, digits=3),
                     tickvals  = ticks,
                     tickmode = "array"
                     )

    my.error.bars = list(type = "data", symmetric = FALSE, color = '#333333',arrayminus = ~dd$low-0.5,array = ~dd$high-0.5)

    hover.text = paste(dd$Names, " AUC ",round(dd$auc,2)," high: ", round(dd$high,2), "low", round(dd$low,2))

    layout_title <- 
        glue::glue( "LEGEND: {input$selectedFactor} {selectedFactorLabels()}" )

    dd$color <- map_chr(dd$auc, ~ ifelse(. < 0.5, "pink", "lightblue"))
    p <- dd %>%
      plot_ly( ) %>%
      add_bars( x = ~dd$heights,
                y = ~dd$Names,
                # need to specify the legend labels
                color = ~dd$color,
                #name = "Enriched",
                orientation = "h",
                hoverinfo = "text",
                text = hover.text,
                showlegend = FALSE
                # , marker = list(color = ~dd$color)
                ) %>%
      add_markers( x = ~dd$low - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE ) %>%
      add_markers( x = ~dd$high - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE,
                   hoverinfo = "none" ) %>%
      add_segments( x = ~dd$low - 0.5,
                   xend = ~dd$high - 0.5,
                   y = ~dd$Names,
                   yend = ~dd$Names,
                   color = toRGB("black"),
                   showlegend = FALSE,
                   hoverinfo = "none") %>%
      layout(title = layout_title,
             xaxis = my.xaxis,
             margin = list(l=120,t=50,b=30,unit="pt",pad=2),
             yaxis = list(title=""))

    p
  })

  ### output$analyzeTable ----
  output$analyzeTable <- DT::renderDataTable({
       d <- roundedData()
       DT::datatable(roundedData(), options = list(pageLength = 5)) 
  })

  # output$filteredPlotly ----
  output$filteredPlotly <- renderPlotly({
    generatePlot_ly()
  })

  output$description<- renderText({
      method <- ifelse(is.null(input$adj_method), "--", input$adj_method)
      thres <- ifelse(is.null(input$significance_threshold), "--", input$significance_threshold)
      fac <- ifelse(is.null(input$selectedFactor), "--", input$selectedFactor)
      s <- glue("Method: <b>{method}</b> Threshold {thres} Factor {fac}")
      s
  })

  ###-- panel at bottom of Analyze panel
  output$dataTabPanel = renderUI({
    validate( need(!is.null(otut_for_processing) &&
                  !is.null(input$selectedFactor),
                  "No data - Data and meta data must be loaded"))

    tabsetPanel(type="tabs",
      tabPanel("Plot", plotlyOutput(ns("filteredPlotly"))),
      tabPanel("Table", DT::dataTableOutput(ns("analyzeTable"))))
  })
  })
}
    
    
## To be copied in the UI
# mod_analyze_ui("analyze_ui_1")
    
## To be copied in the server
# callModule(mod_analyze_server, "analyze_ui_1")

