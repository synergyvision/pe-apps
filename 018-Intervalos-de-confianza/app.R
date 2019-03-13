ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.2.0")
ensure_version("readxl", "1.2.0")
ensure_version("shinydashboard", "0.7.1")
ensure_version("psych", "1.8.10")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.1.0")

library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(matrixStats)
library(ggplot2)


ui <- fluidPage(

  titlePanel("Intervalos de confianza"),
  sidebarLayout(

    sidebarPanel(width = 3,

  selectInput(inputId = 'ic',label = 'Escoja el intervalo de confianza deseado',choices = c('Media de una población','Proporción en una población','Varianza de una población','Diferencia de medias de dos poblaciones'),
              selected = NULL),
  conditionalPanel(condition = "input.ic=='Media de una población'",selectInput(inputId = 'vc',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
  conditionalPanel(condition = "input.ic=='Diferencia de medias de dos poblaciones'",selectInput(inputId = 'vc1',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL))
    ),

  mainPanel(

    )
  )
)



server <- function(input, output,session) {


  }


shinyApp(ui = ui, server = server)
