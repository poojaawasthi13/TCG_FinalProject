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
  titlePanel("Average Residential, Commercial and Industrial Electricity rates by Zip Code across the States"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("SliderX","Commercial + Industrial Rate",-1,1, value = 50),
      sliderInput("SliderY","Residential Sample ",-1,1, value = 50),
      
      submitButton("Submit")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      h3("Electricity rate predicted values:"),
      textOutput("prediction_1")
      
      
    )
  )
))

  
  
  


