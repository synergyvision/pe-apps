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
      numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de que salga cara'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
      actionButton(inputId = 'boton',label='GO!')
      ),

    mainPanel(
      
      fluidRow(
        column(width=12,imageOutput('imagen'),style="text-align: center;"),
      verbatimTextOutput('hola'))
    )
  )
)



server <- function(input, output,session) {
  prueba<-eventReactive(input$boton,{
    rbinom(1,1,input$proba)
  })
  
  output$imagen<-renderImage({
    if(prueba()==1){
    list(src='www/img/moneda1.jpg',height=200,width=200)
    } else{
      list(src='www/img/moneda2.jpg',height=200,width=200)
      }
  },deleteFile = FALSE)

   output$hola<-renderPrint({
     return(prueba())
   })
  }


shinyApp(ui = ui, server = server)
