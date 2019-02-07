ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.2.0")
ensure_version("readxl", "1.2.0")
ensure_version("shinydasboard", "0.7.1")
ensure_version("psych", "1.8.10")
#ensure_version("modeest", "2.3.2")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.1.0")




library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
#library(modeest)
library(matrixStats)
library(ggplot2)


ui <- fluidPage(
  
  titlePanel("Pruebas de Hipotesis"),
  selectInput(inputId = 'ph',label = 'Escoja la Prueba de Hipótesis deseada',choices = c('Media de una población','Diferencia de medias de dos poblaciones',
                                                                                         'Varianza de una población','Igualdad de varianzas de dos poblaciones','Proporción en una población'),
              selected = NULL),
  conditionalPanel(condition = "input.ph == 'Media de una población'", selectInput(inputId = 'vc',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza conocida'", selectInput(inputId = 'tp',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza desconocida'", selectInput(inputId = 'tp1',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones'", selectInput(inputId = 'vc1',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza conocida'", selectInput(inputId = 'tp',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza desconocida'", selectInput(inputId = 'tp1',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Varianza de una población'", selectInput(inputId = 'tp2',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Igualdad de varianzas de dos poblaciones'", selectInput(inputId = 'tp3',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Proporción en una población'", selectInput(inputId = 'tp4',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL))
)



server <- function(input, output,session) {
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
