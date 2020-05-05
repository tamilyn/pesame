
shinyServer(function(input, output, session) {

    source("helper.R")
    source("pesame.R")

    mainData <- reactiveVal(list())
    mainFileDescription <- reactiveVal(list())
    metaFileDescription <- reactiveVal(list())
    filesData <- reactiveVal(list())

    loadedFilename <- reactiveVal(NULL)
    loadedFileExtension <- reactiveVal(NULL)
    loadedFileData <- reactiveVal(NULL)
    loadedRawFileData <- reactiveVal(NULL)

    metadataFilename <- reactiveVal(NULL)

    availableFactors <- reactiveVal(list())
    allOriginalFactor <- reactiveVal(list())
    computedDetails <- reactiveVal(list())

    metadata <- reactiveVal(NULL)
    mvdata <- reactiveVal(NULL)

   orig_metadata <- reactiveVal(NULL)
   ###-- END reactive values ----

    
# load panel --------------------------------------------------------------
    
    descriptions <- reactive({
        current = ""
        mfd <- mainFileDescription()
        fn <- mfd$filename
        if(!is.null(fn)) {
           current = p("file is ", mfd$filename, " ext ", mfd$ext, " num rows ", mfd$num_rows)
        }
        current
    })


    descriptions1 <- reactive({
        current = ""
        mfd <- metaFileDescription()
        fn <- mfd$filename
        if(!is.null(fn)) {
           current = p("META file is ", fn, " ext ", mfd$ext, " num rows ", mfd$num_rows)
        }
        current
    })


    output$progressBox <- renderInfoBox({
      infoBox("Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple")
    })

    xxxxmetaBox <- reactive({
      mfd <- metaFileDescription()
      fn <- mfd$filename
      s1 <- paste0("META file is ", fn, " ext ", mfd$ext, " num rows ", mfd$num_rows)
      infoBox("Metadata", paste0(mfd$num_rows), icon = icon("list"), color = "purple")
    })


    makeBox <- function(mfd, s) {
      if(!is.null(mfd$filename)) {
        xicon = icon("thumbs-up", lib = "glyphicon")
        xcolor = "green"
      } else {
        xicon = icon("list")
        xcolor = "yellow"
      }
      s1 <- paste0(s, mfd$filename, " ext ", mfd$ext, " num rows ", mfd$num_rows)
      infoBox(s, paste0(mfd$num_rows), icon = xicon, color = xcolor)
    }

    mainBox <- reactive({
      mfd <- mainFileDescription()
      makeBox(mfd, "Data")
    })

    metaBox <- reactive({
      mfd <- metaFileDescription()
      makeBox(mfd, "Meta")
    })

    output$loadUI <- renderUI({

        tagList(
            div(id = "main_content",
                p("Load files.  You can load data and metadata as separate csv files, 
                  or an Excel spreadsheet with tabs named otut and f, or a phyloseq
                  data file."),
                box(descriptions()),
                box(descriptions1()),
                metaBox(),
                mainBox(),
                fileInput("file", 'File')))
    })
    

# factor panel ------------------------------------------------------------

 selectedFactorValues <- reactive({
   orig_metadata() %>% pull(input$selectedViewFactor) %>% unique
 })

   output$factorSelection <- renderUI({
    af <- computedDetails()
    if(!is.null(af) && ("name" %in% names(af))) {
       #print(head(af,2))
       af <- af %>% pull(name)
       selectInput('selectedViewFactor', 'Factor', af )
    } else {
      p("no factors available")

    output$loadUI <- renderUI({

        tagList(
            div(id = "main_content",
                p("Load files.  You can load data and metadata as separate csv files, 
                  or an Excel spreadsheet with tabs named otut and f, or a phyloseq
                  data file."),
                box(descriptions()),
                box(descriptions1()),
                metaBox(),
                mainBox(),
                fileInput("file", 'File')))
    })
    

# factor panel ------------------------------------------------------------

 selectedFactorValues <- reactive({
   orig_metadata() %>% pull(input$selectedViewFactor) %>% unique
 })

   output$factorSelection <- renderUI({
    af <- computedDetails()
    if(!is.null(af) && ("name" %in% names(af))) {
       #print(head(af,2))
       af <- af %>% pull(name)
       selectInput('selectedViewFactor', 'Factor', af )
    } else {
      p("no factors available")
    }
   })


 output$selectFactorButtons <- renderUI({
   validate( need(input$selectedViewFactor,
                  "No factors to show because no meta data loaded"))

    selected <- input$selectedViewFactor

    details <- allOriginalFactor()
    kind <- details %>%
            dplyr::filter(name == input$selectedViewFactor) %>%
            dplyr::pull(type)

    uniq_vals <- orig_metadata() %>% dplyr::pull(selected) %>% unique

    if(kind=="numeric") {
     tagList(
        actionButton("applyMean", "Mean"),
        actionButton("applyMedian", "Median"))
    } else {
      tagList(
        #need to initialize with current settings
        checkboxGroupInput("level0checkboxes",
          "Which value should be level 0?", selectedFactorValues()))
    }
 })


 output$selectedFactorDescription <- renderUI({
   if(is.null(input$selectedViewFactor)) {
     return(NULL)
   }

   selected <- input$selectedViewFactor
   af <- computedDetails()

   this_factor <- af %>% filter(name == selected)
   if(this_factor$type == "numeric") {
      return(NULL)
   }

   fvals <- str_split(this_factor$unique_values, ",") %>% unlist

   # based on type of factor
   tagList(
     textInput("level0Label", "Level 0 Label", fvals[1]),
     textInput("level1Label", "Level 1 Label", fvals[2]),
     div(actionButton("applyFactorGroupButton","Apply")))
 })


    output$factorUI <- renderUI({
        tagList(
            fluidRow(column(4, uiOutput("factorSelection")),
                     column(4, uiOutput("selectFactorButtons")),
                     column(4, uiOutput("selectedFactorDescription" ))),
            uiOutput("fp_factorVariables"))
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

  theSelectedFactor <- reactive({
    req(input$selectedFactor, metadata())

    flog.info(str_c("selected factor:[", input$selectedFactor, "]"))

    fd <- metadata()
    if(!input$selectedFactor %in% names(fd)) {
      flog.info("theSelectedFactor not available")
      return(NULL)
    }

    fd %>% pull(input$selectedFactor)
  })


   selectedFactorLabels <- reactive({
     req(input$selectedFactor)

    this_factor <- computedDetails() %>%
            filter(name == input$selectedFactor)
    this_factor$labels
  })


  # use plot_ly to display data
  # generatePlot_ly ----
  generatePlot_ly <- function(dd) {
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

    hover.text = paste(dd$Names, " Auc ",round(dd$auc,2)," high: ", round(dd$high,2), "low", round(dd$low,2))


    layout_title <- str_c( "LEGEND: ", selectedFactorLabels() )

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
  }

  # output$filteredPlotly ----
  output$filteredPlotly <- renderPlotly({
    req(input$signficance_threshold, input$selectedFactor)

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

    generatePlot_ly(dd)
  })

  otut_for_processing <- reactive({
    tft <- mvdata()
    if(is.null(tft)) {
      return(NULL)
    }
    return(as.matrix(tft))
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    req(input$signficance_threshold)

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

    flog.info(str_c("72: baseFilteredData otut null?", is.null(otut),
                    "dim", str_c(dim(otut), collapse=" , "), sep =  " "))
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
    thres <- input$signficance_threshold
    flog.info(str_c("78: after calling data_by_auc",
       "ft dim", str_c(dim(ft), collapse=" , "),
       "significance threshold: ", thres,
       sep =  " "))

    if(thres != "All") {
      thres <- as.numeric(input$signficance_threshold)
      filt = (ft["p.adjust", ] < thres) & !is.na(ft["p.adjust", ])
      result <- t(ft[, filt, drop=F])
    } else {
      result <- t(ft)
    }
    result
  })


  ############################################################
  # generateTicks ----
  # generates sequence for tick marks
  generateTicks <- function(dd) {
    tickMarkers = round(min(0.5, max(abs(c(dd$high, dd$low) - 0.5))), 2)
    seq(-tickMarkers, tickMarkers, length.out = 5)
  }

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


  ### output$analyze_table ----
  output$analyze_table <- renderDataTable({
     #print("ANALYZE TABLE")
     fd <- filteredData() %>%
      mutate(auc = round(auc,5),
             se = round(se,5),
             low = round(low,5),
             high = round(high,5),
             p.value = round(p.value,5),
             p.adjust = round(p.adjust,5))
     #print(head(fd,2))
     #print("iiiiiiANALYZE TABLE")

#     DT::datatable(filteredData(), options = list(
#        rowCallback = JS(
#           "function(row, data) {",
#               "var num = Math.round(data[3]*100)/100 ;",
#               "$('td:eq(3)', row).html(num);",
#    "}")))

     #DT::datatable(filteredData())
     #filteredData()
     fd
  })


  ###-- panel at bottom of Analyze panel
  output$dataTabPanel = renderUI({
   validate( need(!is.null(otut_for_processing) &&
                  !is.null(input$selectedFactor),
                  "No data - Data and meta data must be loaded"))

    tabsetPanel(type="tabs",
      tabPanel("Plot", plotlyOutput("filteredPlotly")),
      tabPanel("Table", dataTableOutput("analyze_table")))
  })
    
    output$analyzeUI <- renderUI({
      tagList(
   div(id="analyze_has_data", 
    fluidRow(
      column(4, selectInput("adj_method",
		label = "Comparison Method", choices = method.options,
		selectize = FALSE,
		size = 1,
      selected = method.options.default)),
      column(4, selectInput(inputId = "signficance_threshold",
		label= "Significance Threshold",
                selectize = FALSE,
		size = 1,
		choices = significance.options,
                selected = significance.options.default )) ,
      column(4,  uiOutput("factorVariables") )),
    uiOutput("dataTabPanel")))

    })

    output$citationUI <- renderUI({
        h1("citation panel")
    })

  setDetails <- function(details) {
       rdetails = details %>% dplyr::filter(ready)
       allOriginalFactor(details)
       availableFactors(rdetails$name)
  }

    setmetadata <- reactive({ 
      df <- loadedFileData()
      fn <- loadedFilename()

      if(is.null(df)) {
        print("DF IS NULL")
        return
      }
      print(str_c("metadata filename: ", fn, " #rows ", nrow(df)))

      metadataFilename(fn)

      metadata(df)
      orig_metadata(df)

      # reset everything to empty
      availableFactors(list())
      allOriginalFactor(list())
      computedDetails(list())

    #print(head(df,2))
    if(ncol(df) >= 1) {
       tryCatch({
          factors_df <- identify_factors(df)
          setDetails(factors_df)
          computedDetails(factors_df)
       }, warning = function(w) {
          flog.warn(str_c("identify factors warning ", w))
       }, error = function(e) {
          flog.error(str_c("identify factors warning ", e))
          browser()
       })
    }
    })

    userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)
      print(glue::glue("userFile(): setting filename {input$file$name} ext {ext}"))

      loadedFilename(input$file$name)
      loadedFileExtension(ext)

      flog.info(str_c("userFile:", ext, input$file$name, sep = " "))
      input$file
    })

    readcsv <- reactive({
      d = data.table::fread(userFile()$datapath)
      as.data.frame(d)
    })

    observeEvent(input$file, {
       # look at the file type and figure out what it is and save it
       ff <- userFile()
       fn <- loadedFilename()
       if(loadedFileExtension() == "csv") {
         df1 <- readcsv()
         loadedRawFileData(df1)
         df <- select(df1, -1)
         loadedFileData(df)
         print(glue::glue("loaded file {nrow(df)} ext {loadedFileExtension()}"))
         #print(head(df,3))


         if(!str_detect(fn, "otut")) {  
            setmetadata() 
            metaFileDescription(list(num_rows = nrow(df), 
                                     ext = loadedFileExtension(),
                                     filename = fn,
                                     df = df))
         } else {
            mainFileDescription(list(num_rows = nrow(df), 
                                     ext = loadedFileExtension(),
                                     filename = fn,
                                     df = df))
            mainData(df)
            mvdata(df)
         }
       } else {
         print("Not csv, now try to load the Excel")
       }
    })
})
