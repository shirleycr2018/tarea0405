#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

library(shiny)
library(shinydashboard)
library(plotly) 
library(dygraphs)
library(shinyWidgets)   

header   <- dashboardHeader(title = "Tarea 04")
sidebar  <- dashboardSidebar()
body     <- dashboardBody(
    
 
    fluidRow(
        mainPanel(
        
                selectInput(inputId="indicadores",
                            choices=c("House Prices in the UK since 1953" ,
                                      "Bank of England Interest Rate"),
                            label="Indicadores", 
                            selected="House Prices in the UK since 1953")

        )
    ),
    fluidRow(
   
               dygraphOutput("dygraphhistoric")
      ),
    fluidRow( 

      column(width=4,
             sliderInput("slider1", label = "Porcentaje Entrenamiento:", min = 60,  max = 80, value = 70))
      , 
      column(width=4,
             sliderInput("slider2", label = "Intervalo de Confianza:", min = 80,  max = 99, value = 95))
      ,

#      column(width = 4,
#             actionBttn(
#               inputId = "bttn1",
#               label = "Predict",
#               color = "primary",
#               size = "md",
#               style = "unite"
#             )
#          )
   
    ),
    
    fluidRow( dygraphOutput("dygrapHW")),
    fluidRow(dygraphOutput("dygrapAR"))
  
)

dbpage   <- dashboardPage( 
    skin = "blue",
    header,
    sidebar,
    body 
)

# Define UI for application that draws a histogram
shinyUI(
    dbpage
)
