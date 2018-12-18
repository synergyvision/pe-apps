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
      actionButton('boton','GO!')
      ),

    mainPanel(
      
      #imageOutput('imagen')
      verbatimTextOutput('hola')
    )
  )
)



server <- function(input, output,session) {
  prueba<-eventReactive(input$boton,{
    rbinom(1,1,0.5)
  })
  
  output$hola<-renderPrint({
    return(prueba())
  })

  
  }


shinyApp(ui = ui, server = server)
