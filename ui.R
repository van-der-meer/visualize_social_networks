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
  
  # Título principal de la página
  titlePanel("Visualizar redes sociales"),
  
  # Fila para subir archivo y opciones relacionadas
  fluidRow(
    column(4,  
           fileInput("file", "Subir archivo CSV:",
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
           ), 
           downloadButton("download_image", "Descargar gráfico en PDF")
    ), 
    
    column(4,  
           selectInput(inputId = "network_type",
                       label = "Seleccionar tipo de procesamiento",
                       c("Original" = "raw", 
                         "Sumada" = "summed"
                       )
           ),
           checkboxInput("positive", "Mostrar valores positivos", value = TRUE),
           checkboxInput("negative", "Mostrar valores negativos", value = TRUE),
           checkboxInput("consistent_layout", "Mantener diseño consistente", value = TRUE),
    ), 
    
    column(4,  
           # Control deslizante para umbral de conexiones
           sliderInput("threshold", 
                       "Umbral para las conexiones", 
                       min = 0, 
                       max = 6, 
                       value = 0, 
                       step = 1), 
           
           # Control deslizante para tamaño de nodos
           sliderInput("node_size", 
                       "Tamaño de los nodos", 
                       min = 0, 
                       max = 2, 
                       value = 1, 
                       step = 0.1),
           
           # Opciones para seleccionar estudiante individual
           checkboxInput("individual", "Seleccionar estudiante"),
           
           conditionalPanel(
             condition = "input.individual == true",
             selectInput("student_id", "Estudiante", choices = list(1, 2, 3))
           ),
           
           conditionalPanel(
             condition = "input.individual == true",
             checkboxInput("ingoing_conns", "Mostrar conexiones entrantes", value = TRUE)
           ),
           
           conditionalPanel(
             condition = "input.individual == true",
             checkboxInput("outgoing_conns", "Mostrar conexiones salientes", value = TRUE)
           )
    ), 
  ),
  
  # Fila para el botón de información
  fluidRow(
    column(12, 
           actionButton("action_info", "Información"), 
           htmlOutput("information")
    ),
  ),
  
  hr(),
  
  # Panel principal para mostrar el gráfico y la tabla de datos
  mainPanel(
    plotOutput("graph"),
    tableOutput("data")
  )
)
