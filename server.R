#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(plyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(cluster)
library(xml2)



#urllink <- read.csv("https://raw.githubusercontent.com/poojaawasthi13/TCG_FinalProject/master/noniouzipcodes2015.csv",header=TRUE, sep=",")
#fn <- data.frame(urllink)


shinyServer(function(input, output) {
  urllink <- read.csv("https://raw.githubusercontent.com/poojaawasthi13/TCG_FinalProject/master/noniouzipcodes2015.csv",header=TRUE, sep=",")
  #fn <- data.frame(urllink)
  
  model_1 <- lm(ind_rate ~ comm_rate+ res_rate, data = urllink) 
  
  model_1prediction <- reactive({
    commInput <- input$SliderX
    resInput <- input$SliderY
    
    
    predict(model_1, newdata = data.frame(comm_rate = commInput,
                                         res_rate = resInput
                                         
    ))
  })
  urllink$residuals <- residuals(model_1)
  
  comm <- reactive({
    commInput <- input$SliderX
  })
  
  output$distPlot <- renderPlot(
    
    ggplot(urllink, aes(x = comm_rate, y = residuals)) +
      geom_point(colour = "black")  +
      geom_segment(aes(xend = comm_rate, yend = 0), alpha = .01)  
    + geom_point(aes(x=comm(), y = 0), colour = "green")
    
  )
  
  output$prediction_1 <- renderText({
    model_1prediction()
  })    
  
  
  
})


