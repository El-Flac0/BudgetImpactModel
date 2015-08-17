## Server
library(shiny)
require(dplyr)
require(reshape2)
require(RColorBrewer)
require(scales)
require(ggplot2)
source('helper.R')

shinyServer(function(input, output) {
  
  output$table1 <- renderTable({
    
    share <- input$slider1
    
    marketShare(share=share) 
    
  }, include.rownames=FALSE)
  
  output$plot1 <- renderPlot({
    
    incidence <- input$text1
      
    popSize <- input$text2
      
    treat <- input$text3
    
    share <- input$slider1
      
    impact <- switch(input$radio1,
                     "Absolute" = "absolute",
                     "Incremental" = "incremental")
      
    plot <- switch(input$radio2,
                   "Stratified by Year" = "stratified",
                   "Aggregated over Period" = "aggregated")
      
    stacked <- input$check1
    
    plotMaker(incidence=incidence, 
              popSize=popSize, 
              treat=treat,
              share=share,
              impact=impact,
              plot=plot, 
              stacked=stacked)
    
  })
  
})
