#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Load the functions
  source('pesame.R')
  load("example_data.RData")   # creates data 'otut' and factor value 'f' 
  source('helper.R')
  #sink("sink-example.txt")
  
  helper.setpar()  # set plot parameters
  
  output$signficant_threshold <- 
    reactive({ input$signficance_threshold })
  
  # Generate a summary of the data
  filteredData <- reactive({ 
    ft <- helper.data_by_auc(otut,f,input$adj_method)
    filt = (ft["p.adjust",]< input$signficance_threshold) & !is.na(ft["p.adjust",])
    ft[,filt,drop=F]
  })
  
  drawPlot <- function(d) {
    lims = round(min(0.5, max(abs(c(d["high",], d["low",])-0.5))), 2)
    
    bar.lengths = d["auc",,drop=F]-0.5
    cols = c("red", "green")[(bar.lengths>0.0)+1]
    ys = barplot(bar.lengths,
                 col = cols,
                 border="black",
                 horiz=TRUE, 
                 axes=FALSE,
                 cex.names=0.5,
                 xlim=c(-lims, lims),
                 names.arg = colnames(bar.lengths),
                 xlab="AUC")
    ticks = 5
    ticks = seq(-lims, lims, length.out=ticks)
    labels = format(ticks+0.5, digits=3)
    axis(1, at= ticks, labels=labels)
    # Show the factors at the top of the chart
    legend("topleft", bty="n",legend=levels(f)[1], text.col='black', horiz=T,text.width=30)
    legend("topright", bty="n", legend=levels(f)[2], text.col='black', horiz=T)
    segments(d["low",] - 0.5, ys, d["high",] - 0.5, ys, col="gray")
    abline(v=0, lwd=5)
  }
  
  output$filteredPlot <- renderPlot({
    drawPlot(filteredData())
  })
  
  output$filterSummary <- renderTable({ data.frame(filteredData()) })
  
})
