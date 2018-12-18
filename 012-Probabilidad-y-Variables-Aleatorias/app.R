ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydasboard", "0.7.0")
ensure_version("psych", "1.8.4")
ensure_version("modeest", "2.1")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.0.0")




library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(modeest)
library(matrixStats)
library(ggplot2)




ui <- fluidPage(
  
  titlePanel("Lanzamiento de moneda"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = 'monedas',label = HTML('Seleccione la cantidad <br/>de monedas a lanzar'),choices = c('1','2','3','4','5'),selected = '1'),
      numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de salir cara'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
      actionButton(inputId = 'boton',label='GO!')
      ),

    mainPanel(
      
      fluidRow(
        conditionalPanel(condition = "input.monedas=='1'",
        column(width=12,imageOutput('imagen1'),style="text-align: center;")),
        conditionalPanel(condition = "input.monedas=='2'",column(width=6,imageOutput('imagen2')),
                         column(width=6,imageOutput('imagen3'))),
      verbatimTextOutput('hola'))
    )
  )
)



server <- function(input, output,session) {
  prueba<-eventReactive(input$boton,{
    rbinom(as.numeric(input$monedas),1,input$proba)
  })
  
  output$imagen1<-renderImage({
      if(prueba()==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
      } else{
      list(src='www/img/moneda2.jpg',height=200,width=200)
      }
  },deleteFile = FALSE)
  

  output$imagen2<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
    } else if(prueba()[1]==0){
      list(src='www/img/moneda2.jpg',height=200,width=200)
    }
  },deleteFile = FALSE)
  
  output$imagen3<-renderImage({
    if(prueba()[2]==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
    } else if(prueba()[2]==0){
      list(src='www/img/moneda2.jpg',height=200,width=200)
    }
  },deleteFile = FALSE)
  
   output$hola<-renderPrint({
     return(prueba())
   })
  }


shinyApp(ui = ui, server = server)
