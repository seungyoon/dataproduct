#
# Auther : Seungyoon Lee
# Date   : Sep 19, 2016
#
# This is the server logic of a Shiny web application. 
# this will prepare ozonePlot to draw Temp vs Ozone or Wind vs Ozone per user
# selection from the ui.R. Also it will draw trendline if user checked it.
# 
# this file provides document text to Ui.R to give basic idea that this application
# provides.
# 
#

library(shiny)
library(ggplot2)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  selectedData <- reactive({
      temp <- airquality[, c(input$variable, "Ozone")]
      # return complete case only from selectedData by removing NA values
      temp[complete.cases(temp),]
  })
    
  
  output$ozonePlot <- renderPlotly({
      if (input$variable == "Temp") {
          completeDf <- selectedData()
          
          # aggregate to get mean by Temp
          meanDf <- aggregate(completeDf[,2], list(completeDf$Temp), mean)
          colnames(meanDf) <- c("Temp", "Ozone")
          
          # get freqeuncy of Temp
          freq <- table(completeDf$Temp)
          freqDf <- as.data.frame(freq)
          colnames(freqDf) <- c("Temp", "Freq")
          
          # merge them (complete set + freq : Temp, mean of Ozone, Freq)
          cdf <- merge(meanDf, freqDf, by="Temp")
          
          # Plot
          if (input$showTrend) {
              fit <- lm(Ozone ~ Temp, data = cdf)
              plot_ly(cdf, x = Temp, y = Ozone, mode = "markers", color = Ozone, size = Freq) %>%
                  layout(title = "mean of Mean Ozone in parts per billion (ppb)") %>%
                  add_trace(data = cdf, x = Temp, y = fitted(fit), mode = "lines") %>%
                  layout(showlegend = FALSE)
              
          } else {
              plot_ly(cdf, x = Temp, y = Ozone, mode = "markers", color = Ozone, size = Freq) %>%
                  layout(title = "mean of Mean Ozone in parts per billion (ppb)")
          }
      } else if (input$variable == "Wind") {
              completeDf <- selectedData()
              
              # aggregate to get mean by Wind
              meanDf <- aggregate(completeDf[,2], list(completeDf$Wind), mean)
              colnames(meanDf) <- c("Wind", "Ozone")
              
              # get freqeuncy of Wind
              freq <- table(completeDf$Wind)
              freqDf <- as.data.frame(freq)
              colnames(freqDf) <- c("Wind", "Freq")
              
              # merge them (complete set + freq : Wind, mean of Ozone, Freq)
              cdf <- merge(meanDf, freqDf, by="Wind")
              
              # Plot
              if (input$showTrend) {
                  fit <- lm(Ozone ~ Wind, data = cdf)
                  plot_ly(cdf, x = Wind, y = Ozone, mode = "markers", color = Ozone, size = Freq) %>%
                      layout(title = "mean of Mean Ozone in parts per billion (ppb)") %>%
                      add_trace(data = cdf, x = Wind, y = fitted(fit), mode = "lines") %>%
                      layout(showlegend = FALSE)
              } else {
                  plot_ly(cdf, x = Wind, y = Ozone, mode = "markers", color = Ozone, size = Freq) %>%
                      layout(title = "mean of Mean Ozone in parts per billion (ppb)")
              }
      }
  })
  
  output$text <- renderText({
      paste("the built-in [R] airquality data set has Ozone data with Solar, Wind and Temperatere From May 1, 1973 to Sep 30, 1973.",
            "",
            "This Shiny app will load that data set and plot the Ozone data per Wind or Temperature, and provide the trend line of the corrected data by the selection.",
            "",
            "Selecting the variable from left drop box will pick the x-axis to draw Ozone value per the selection.",
            "",
            "- selecting Wind from the left drop box will give plot of Ozone ~ Wind",
            "- seleting Temperate from the left drop box will plot of Ozone ~ Temp",
            "",
            "Checking the box for trend will draw trend line on top of above plot", sep="\n")
  })
  
})
