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
  
  titlePanel("Distribución Normal"),
  tabsetPanel(type = 'pills',id='pri',
              tabPanel('Características',includeHTML("normal.html")),
              tabPanel('Cálculos',br(),selectInput(inputId = 'ber',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Comparación de Medias','Comparación de Varianzas','Cuantiles','Muestra Aleatoria','Normal estándar'),selected = NULL)))
)



server <- function(input, output,session) {
  
  
  }


shinyApp(ui = ui, server = server)
