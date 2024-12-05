# install required packages
list.of.packages <- c("shiny", "dplyr", "tidyr", "qgraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
library(shiny)
library(dplyr)
library(qgraph)
library(tidyr)

fluidPage(
  
  titlePanel("Visualize social networks"),

  #hr(style="border-top: 1px solid #000000;"), 
  #br(), 

  fluidRow(
    column(4,  
           fileInput("file", "Upload CSV file:",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ), 
            downloadButton("download_image", "Download graph as pdf")

    ), 
    column(4,  
           selectInput(inputId = "network_type",
                       label = "Choose processing",
                       c(Raw = "raw", 
                         Summed = "summed"
                       )
           ),
           checkboxInput("positive", "Display positive values", value = TRUE),
           checkboxInput("negative", "Display negative values", value = TRUE),
           checkboxInput("consistent_layout", "Keep layout consistent", value = TRUE),
    ), 
    column(4,  
           # choose threshold for display
           sliderInput("threshold", 
                      "Threshold for connections", 
                      min = 0, 
                      max = 6, 
                      value = 0, 
                      step = 1), 
           sliderInput("node_size", 
                       "Size of the dots", 
                       min = 0, 
                       max = 2, 
                       value = 1, 
                       step = 0.1),
           checkboxInput("individual", "Select Student"),
           conditionalPanel(
             condition = "input.individual == true",
             selectInput("student_id", "Student", choices = list(1, 2, 3))
           ),
           conditionalPanel(
             condition = "input.individual == true",
             checkboxInput("ingoing_conns", "Show ingoing connections", value = TRUE)
             ),
           conditionalPanel(
             condition = "input.individual == true",
             checkboxInput("outgoing_conns", "Show outgoing connections", value = TRUE)
             )
             
    ), 
  ),
  
  fluidRow(
    column(12, 
           actionButton("action_info", "Information"), 
           htmlOutput("information")
    ),
  ),
  
  hr(),

  mainPanel(
    plotOutput("graph"),
    tableOutput("data")
  )
  
)