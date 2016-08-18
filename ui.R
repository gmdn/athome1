library(tm)
library(dplyr)
library(ggplot2)
library(magrittr)
library(Matrix)
library(SnowballC)
library(httr)
library(jsonlite)
library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Athome1 Total Recall"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(width = 6,
               textInput(inputId = "runid",
                         label = h5("runid"),
                         value = "ZkC36NYlNfiw",
                         placeholder = "Enter runid...")
        ),
        column(width = 6, 
               selectInput(inputId = "topics",
                           label = h5("topid"),
                           choices = topics$topid,
                           width = "100%")
        )
      ),
      
      textInput(inputId = "query",
                label = h5("query"),
                value = ""),
      
      fluidRow(
        column(width = 4,
               actionButton(inputId = "search", label = "search")
        ),
        column(width = 4,
               sliderInput(inputId = "topk",
                           label = NULL, #h4("top k"),
                           min = 100, max = 1000, value = 100, step = 100)
        ),
        column(width = 4,
               textOutput(outputId = "outSearch")
        )
      ),
      

      
      
      
      fluidRow(
        column(width = 6, actionButton(inputId = "judge", label = "judge")
        ),
        column(width = 6, tableOutput(outputId = "outJudge")
        )
      ),
      
      sliderInput(inputId = "range",
                  label = "range",
                  min = -5000,
                  max = 100,
                  value = c(-1000, 100),
                  step = 100),
    
      sliderInput(inputId = "features",
                  label = "features",
                  min = 100,
                  max = 10000,
                  value = 10000,
                  step = 100)
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      #verbatimTextOutput("query"),
      #verbatimTextOutput("qrels"),
      #verbatimTextOutput("coords"),
      
      #verbatimTextOutput("measures"),
      tableOutput("measures"),
      #verbatimTextOutput("judge")
      #verbatimTextOutput("coord"),
      plotOutput("plot"),
      
      fluidRow(
        column(width = 6,
               sliderInput(inputId = "rotate",
                           label = "rotate",
                           min = 0.5,
                           max = 3,
                           value = 1,
                           step = 0.01)
        ),
        column(width = 6,
               sliderInput(inputId = "shift",
                           label = "shift",
                           min = -1000,
                           max = 100,
                           value = 0,
                           step = 5)
        )
      ),
      
      tableOutput("features")

    )
  )
))
