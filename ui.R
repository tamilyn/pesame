#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("PESAME: Predictive effect size analysis in multivariate ensembles"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
  
      radioButtons("adj_method", label = h3("Comparison Method"),
                   choices = list("holm","hochberg","hommel","bonferroni",
                                  "BH","BY", "fdr","none"),
                   selected = "fdr"),
      
      sliderInput(inputId = "signficance_threshold",
                  label = "Significance Threshold",
                  min = 0,
                  max = 1.0,
                  value = 0.5),
      submitButton("Submit")
    ),
    
    # Show tabs
    mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Plot",plotOutput("filteredPlot")),
                tabPanel("Table", tableOutput("filterSummary")),
                tabPanel("Citation", verbatimTextOutput("summary"))
    )))
))

