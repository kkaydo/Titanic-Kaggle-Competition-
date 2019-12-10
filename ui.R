#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
load("C02NorthernHemisphere.Rdata")
load("CanadianAvgSnow.Rdata")
load("CanadianMeanTemp.Rdata")
load("CanadianPrecip.Rdata")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Greenhouse Effect and Climate Change"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "CO2","select latitude:",
        c("Latitude30value","Latitude33value","Latitude37value",
          "Latitude41value","Latitude44value","Latitude49value",
          "Latitude53value","Latitude58value","Latitude64value",
          "Latitude72value","Latitude90value")),
      textInput("city", 
                "Enter a name of city in Canada:(CAP)",
                value = "NULL"),
      selectInput(
        "choice","chose one:",
        c("Temperature","Snow","Precip")
      ),
      conditionalPanel(
        condition="input.choice=='Temperature'",
        checkboxInput("Temp", "add moving average line?",FALSE),
        conditionalPanel(
          condition='input.Temp == true',
          sliderInput("tempSlide",
                      "enter a bandwidth:",
                      min = 1,
                      max = 20,
                      value = 5)
          )
        ),
      conditionalPanel(
        condition="input.choice=='Snow'",
        checkboxInput("snow", "add moving average line?",FALSE),
        conditionalPanel(
          condition='input.snow == true',
          sliderInput("snowSlide",
                      "enter a bandwidth:",
                      min = 1,
                      max = 20,
                      value = 5)
        )
      ),
      conditionalPanel(
        condition="input.choice=='Precip'",
        checkboxInput("precip", "add moving average line?",FALSE),
        conditionalPanel(
          condition='input.precip == true',
          sliderInput("precipSlide",
                      "enter a bandwidth:",
                      min = 1,
                      max = 20,
                      value = 5)
        )
       )
      )
      ,
     
    
    mainPanel(
      tabsetPanel(
        tabPanel("CO2 level",
                 plotOutput("CO2Plot")),
        tabPanel("Climate change",
                 plotOutput("climatePlot")),
        tabPanel("About",
                 textOutput("aboutText"))
       
    
  
   )
  )
 )
)
)
