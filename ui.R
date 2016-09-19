#
# Auther : Seungyoon Lee
# Date   : Sep 19, 2016
#
# This is the user-interface definition of a Shiny web application. 
# User may select x-axis from the drop box and check the box below to see the
# trend line. the data set here is from [R] built-in airquality packages
#

library(shiny)
library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
  
  # Application title
  titlePanel("New York Air Quality Measurements"),
  
  hr(),
  
  plotlyOutput("ozonePlot"),
  
  hr(),

  fluidRow(
      column(width = 4, 
          selectInput("variable", "Variable:",
                      c("Wind (mph)" = "Wind", "Temperature (deg F)" = "Temp")),
          checkboxInput(inputId = "showTrend", label = "show Ozone trend",
                        value = FALSE)
      ),
      column(width = 8,
             verbatimTextOutput("text")
      )
  )
    
))
