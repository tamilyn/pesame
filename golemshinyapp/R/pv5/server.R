
shinyServer(function(input, output, session) {

    source("helper.R")
    source("pesame.R")

    mainData <- reactiveVal(list())
    mainFileDescription <- reactiveVal(list())
    metaFileDescription <- reactiveVal(list())
    filesData <- reactiveVal(list())

    loadedFilename <- reactiveVal(NULL)
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

    makeBox <- function(mfd, s) {
      if(!is.null(mfd$filename)) {
        xicon = icon("thumbs-up", lib = "glyphicon")
        xcolor = "green"
        s1 <- paste0(mfd$filename, "\n", mfd$num_rows, " x ", mfd$num_cols)
      } else {
        xicon = icon("list")
        xcolor = "yellow"
        s1 <- ""
      }
      infoBox(s,  s1, icon = xicon, color = xcolor)
    }

    output$loadUI <- renderUI({

        tagList(
            div(id = "main_content",
                p("Load files.  You can load data and metadata as 
                  separate csv files, 
                  or an Excel spreadsheet with tabs named otut and f 
                  ."),
                makeBox(metaFileDescription(), "Meta"),
                makeBox(mainFileDescription(), "Data"),
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


  ### applyFactorNumeric
  applyFactorNumeric <- function(ft, fmethod) {
    flog.info(str_c("applyFactorNumeric: ", ft, " fmethod ", fmethod))
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

    flog.info("UPDATING applyFactorNumeric computedDetails")
    computedDetails(cd)
    setDetails(details)
  }

  observeEvent(input$applyMean, {
     applyFactorNumeric(input$selectedViewFactor, "mean")
  })

  observeEvent(input$applyMedian, {
     applyFactorNumeric(input$selectedViewFactor, "median")
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
    print("labels")
    print(f_labels)
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


 output$selectedFactorDescription <- renderUI({
   if(is.null(input$selectedViewFactor)) {
     return(NULL)
   }

   selected <- input$selectedViewFactor
   print(glue::glue("188: Selected: {selected}"))
   af <- computedDetails()

   this_factor <- af %>% filter(name == selected)
   print(glue::glue("192: this_factor {this_factor} Selected: {selected}"))
   if(is.null(this_factor) || this_factor$type == "numeric") {
      return(NULL)
   }

   fvals <- str_split(this_factor$unique_values, ",") %>% unlist

   # based on type of factor
   tagList(
     textInput("level0Label", "Level 0 Label", fvals[1]),
     textInput("level1Label", "Level 1 Label", fvals[2]),
     div(actionButton("applyFactorGroupButton","Factor Apply Levels")))
 })


 #########
 #########


 ### output$factorsDataTable
 output$factorsDataTable <- function() {
   
   # factors output table 
   #library(kableExtra)
   #library(skimr)
   

   details <- computedDetails()
   orig_fd <- orig_metadata()

   my_skim <- skim(orig_fd)
   #print(colnames(my_skim))
   #print(my_skim)
   #browser()
   my_skim_1 <- select(my_skim, skim_variable, missing=n_missing, 
      numeric.mean, numeric.sd, numeric.hist)
   #my_skim_1 <- my_skim  %>%
     #dplyr::filter(stat == "hist") %>%
     #select(skim_variable,Histogram=formatted)

   details2 <- details %>%
     left_join(my_skim_1, by=c("name"="skim_variable")) %>%
     select(numeric.hist, name, everything()) %>%
     knitr::kable("html") %>%
     kable_styling("striped", full_width = FALSE)
 }
 ## output$factorVariables ----
 
   output$fp_factorVariables <- renderUI({
     print("fp_factorVariables")
     validate( need(orig_metadata(),
                  "No factors to show because no meta data loaded"))

     print("fp_factorVariables")
     return(tableOutput("factorsDataTable"))
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

    setmetadata <- function(df,fn) { 
      #df <- loadedFileData()
      #fn <- loadedFilename()

      if(is.null(df)) {
        print("DF IS NULL")
        return
      }
      print(str_c("metadata filename: ", fn, " #rows ", nrow(df), " ", ncol(df)))

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
    }

    userFile <- reactive({
      # If no file is selected, do nothing
      validate(need(input$file, message = FALSE))

      ext <- tools::file_ext(input$file$name)
      print(glue::glue("userFile(): setting filename {input$file$name} ext {ext}"))

      loadedFilename(input$file$name)

      flog.info(str_c("userFile:", ext, input$file$name, sep = " "))
      input$file
    })

    readcsv <- function(dp) {
      d = data.table::fread(dp)
      as.data.frame(d)
    }
    observeEvent(input$applyFactorGroupButton, {
      print("--- applyFactorGroupButton")
      applyFactorGroup(input$selectedViewFactor, input$level0checkboxes) 
    })

    observeEvent(input$file, {
       # look at the file type and figure out what it is and save it
       ff <- userFile()
       fn <- loadedFilename()
       dp <- userFile()$datapath

       for(i in seq_along(dp)) { 
            entry <- dp[[i]]
            the_ext <- tools::file_ext(entry)
            the_fn <- fn[[i]]

           if(the_ext == "csv") {
             df1 <- readcsv(entry)
             loadedRawFileData(df1)
             df <- select(df1, -1)
             loadedFileData(df)
             print(glue::glue("loaded file {the_fn} {nrow(df)} {ncol(df)} ext {the_ext}"))
             #print(head(df,3))
             the_list <- list(num_rows = nrow(df), num_cols = ncol(df), ext = the_ext,
                                     filename = the_fn, df = df)

             if(!str_detect(the_fn, "otut")) {  
               setmetadata(df, the_fn) 
               metaFileDescription(the_list)
             } else {
      print(str_c("data filename: ", the_list$filename, " #rows ", nrow(df), " ", ncol(df)))
               mainFileDescription(the_list)
               mainData(df)
               mvdata(df)
            }
          } else {
            print("Not csv, now try to load the Excel")
          }
       } 
    })
})
