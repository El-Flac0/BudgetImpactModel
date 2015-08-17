
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel('Budget Impact Model'),
  
  # Sidebar with a slider input for number of bins
  wellPanel(
    tags$style(type="text/css", '#leftPanel { width:270px; float:left;}'),
    id = "leftPanel",
                
    textInput("text1", label = h5("Disease Incidence"), 
              value = "0.01"),
    textInput("text2", label = h5("Population Size"), 
              value = "1000000"),
    textInput("text3", label = h5("% Treated"), 
              value = "0.8"),
    sliderInput("slider1", label = h5("Year 1 Market Share for X"),
                min = 0.05, max = 0.35, value = 0.05, step = 0.05),
    radioButtons("radio1", label = h5("Budget Impact"),
                 choices = list("Absolute", "Incremental"),selected = 'Absolute'),
    checkboxInput("check1", label = "Resource Utilisation Breakdown", FALSE),
    radioButtons("radio2", label = h5("Bar Plotting"),
               choices = list("Stratified by Year", "Aggregated over Period"),
               selected = "Stratified by Year")
  ),
  
  # Show a table of market shares, and plot of costs
  mainPanel(
    tabsetPanel(
      tabPanel('Main',
               br(),
               p('Welcome to the budget impact webapp for Treatment X, developed by Unnamed MegaCorp'),
               p('Utilise the input sidebar to enter the parameters of the model. Then select
                 the appropriate plotting options and the corresponding results will be plotted.
                 The market share table can be seen in detail on the Market Share tab, with all
                 other data, such as unit costs and quantities for medical resource use and competing
                 treatments, hardcoded into the model.'),
               br(), 
               plotOutput('plot1')
      ),
      tabPanel('Market Share',
               br(),
               p('The market share table will update with the slider in the sidebar.
                 Adjusting by 5% increments will alter market share for all treatments, and
                 associated calculations for the BIM. The Year 1 market share projection is
                 capped at 35%, and a linear increase in market share for subsequent 
                 years is assumed.'),
               br(),   
               tableOutput('table1')
      )
      )   
      ) 
    )
  )

