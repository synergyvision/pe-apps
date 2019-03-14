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

  mainPanel(withMathJax(),width = 9,
            conditionalPanel(condition = "input.ic=='Media de una población' & input.vc=='Varianza conocida'",column(width = 3,numericInput(inputId = 'MediaMuestral',label = HTML('Inserte media de la muestra X&#772;'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'Muestra',label = HTML('Inserte tamaño de la muestra <i>n</i>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'VarianzaPob',label = HTML('Inserte varianza poblacional <i>&sigma;<sup>2</sup></i>'),min=0.1,max = 50,value = 1,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'signif',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',plotOutput('grafica1')))
    )
  )
)



server <- function(input, output,session) {


  output$grafica1<-renderPlot({

  })


  }


shinyApp(ui = ui, server = server)
