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
      radioButtons(inputId = 'monedas',label = HTML('Seleccione la cantidad <br/>de monedas a lanzar'),choices = c('1','2','3','4','5','6','7','8','9','10'),selected = '1'),
      numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de salir cara'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
      actionButton(inputId = 'boton',label='GO!')
      ),

    mainPanel(
      
      fluidRow(
        conditionalPanel(condition = "input.monedas=='1'",
        column(width=12,imageOutput('imagen1',height = "300px"),style="text-align: center;")),
        conditionalPanel(condition = "input.monedas=='2'",column(width=6,imageOutput('imagen2',height = "300px")),
                         column(width=6,imageOutput('imagen3',height = "300px"))),
        conditionalPanel(condition = "input.monedas=='3'",column(width=4,imageOutput('imagen4',height = "300px")),
                         column(width=4,imageOutput('imagen5',height = "300px")),column(width=4,imageOutput('imagen6',height = "300px"))),
        conditionalPanel(condition = "input.monedas=='4'",column(width=5,offset = 1,imageOutput('imagen7',height = "200px")),
                         column(width=5,offset = 1,imageOutput('imagen8',height = "200px")),column(width=5,offset = 1,imageOutput('imagen9',height = "200px")),
                         column(width=5,offset = 1,imageOutput('imagen10',height = "200px"))),
        conditionalPanel(condition = "input.monedas=='5'",column(width=4,imageOutput('imagen11',height = "200px")),
                         column(width=4,imageOutput('imagen12',height = "200px")),column(width=4,imageOutput('imagen13',height = "200px")),
                         column(width=5,offset = 1,imageOutput('imagen14',height = "200px")),column(width=5,offset = 1,imageOutput('imagen15',height = "200px"))),
        conditionalPanel(condition = "input.monedas=='6'",column(width=4,imageOutput('imagen16',height = "200px")),
                         column(width=4,imageOutput('imagen17',height = "200px")),column(width=4,imageOutput('imagen18',height = "200px")),
                         column(width=4,imageOutput('imagen19',height = "200px")),column(width=4,imageOutput('imagen20',height = "200px")),
                         column(width=4,imageOutput('imagen21',height = "200px"))),
      verbatimTextOutput('hola'))
    )
  )
)



server <- function(input, output,session) {
  prueba<-eventReactive(input$boton,{
    rbinom(as.numeric(input$monedas),1,input$proba)
  })
  
  #1 lanzamiento
  output$imagen1<-renderImage({
      if(prueba()==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
      } else{
      list(src='www/img/moneda2.jpg',height=200,width=200)
      }
  },deleteFile = FALSE)
  
  # 2 lanzamiento
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
  
  # 3 lanzamientos
  output$imagen4<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
    } else if(prueba()[1]==0){
      list(src='www/img/moneda2.jpg',height=200,width=200)
    }
  },deleteFile = FALSE)
  
  
  output$imagen5<-renderImage({
    if(prueba()[2]==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
    } else if(prueba()[2]==0){
      list(src='www/img/moneda2.jpg',height=200,width=200)
    }
  },deleteFile = FALSE)
  
  output$imagen6<-renderImage({
    if(prueba()[3]==1){
      list(src='www/img/moneda1.jpg',height=200,width=200)
    } else if(prueba()[3]==0){
      list(src='www/img/moneda2.jpg',height=200,width=200)
    }
  },deleteFile = FALSE)
  
  #4 lanzamientos
  
  output$imagen7<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[1]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen8<-renderImage({
    if(prueba()[2]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[2]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen9<-renderImage({
    if(prueba()[3]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[3]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen10<-renderImage({
    if(prueba()[4]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[4]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  # 5 lanzamientos
  
  output$imagen11<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[1]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen12<-renderImage({
    if(prueba()[2]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[2]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen13<-renderImage({
    if(prueba()[3]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[3]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen14<-renderImage({
    if(prueba()[4]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[4]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen15<-renderImage({
    if(prueba()[5]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[5]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  # 6 lanzamiento
  
  output$imagen16<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[1]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen17<-renderImage({
    if(prueba()[2]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[2]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen18<-renderImage({
    if(prueba()[3]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[3]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  output$imagen19<-renderImage({
    if(prueba()[4]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[4]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen20<-renderImage({
    if(prueba()[5]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[5]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
  output$imagen21<-renderImage({
    if(prueba()[6]==1){
      list(src='www/img/moneda1.jpg',height=150,width=150)
    } else if(prueba()[6]==0){
      list(src='www/img/moneda2.jpg',height=150,width=150)
    }
  },deleteFile = FALSE)
  
  
   output$hola<-renderPrint({
     return(prueba())
   })
  }


shinyApp(ui = ui, server = server)
