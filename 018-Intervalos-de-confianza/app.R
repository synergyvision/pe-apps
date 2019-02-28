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
ensure_version("shinyjs","1.0")

library(shinyjs)
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

  selectInput(inputId = 'ic',label = 'Escoja el intervalo de confianza deseado',choices = c('a','b','c'))
    ),

  mainPanel(
    tabsetPanel(type='tabs',id='pri',
                tabPanel('datos'),
               tabPanel('grafico'))

    )
  )
)



server <- function(input, output,session) {

  # observe({
  #   toggle(condition = input$ic!='b',selector = "#pri li a[data-value=grafico]")
  # })

  observeEvent(input$ic,{
    if(input$ic=='a' |input$ic=='b'){hideTab(inputId = 'pri',target = 'grafico')}else{showTab('pri','grafico')}
  })


  }


shinyApp(ui = ui, server = server)
