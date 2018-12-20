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
  
  titlePanel("Lanzamiento de dados"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = 'dados',label = HTML('Seleccione la cantidad <br/>de dados a lanzar'),choices = c('1','2','3','4','5','6','7','8','9','10'),selected = '1'),
      # numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de salir cara'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
      actionButton(inputId = 'boton',label='GO!')
      ),

    mainPanel(
      
      fluidRow(
        conditionalPanel(condition = "input.dados=='1'",
        column(width=12,imageOutput('imagen1',height = "300px"),style="text-align: center;")),
        conditionalPanel(condition = "input.dados=='2'",column(width=6,imageOutput('imagen2',height = "300px")),
                         column(width=6,imageOutput('imagen3',height = "300px"))),
         conditionalPanel(condition = "input.dados=='3'",column(width=4,imageOutput('imagen4',height = "300px")),
                          column(width=4,imageOutput('imagen5',height = "300px")),column(width=4,imageOutput('imagen6',height = "300px")))),
         conditionalPanel(condition = "input.dados=='4'",column(width=5,offset = 1,imageOutput('imagen7',height = "200px")),
                          column(width=5,offset = 1,imageOutput('imagen8',height = "200px")),column(width=5,offset = 1,imageOutput('imagen9',height = "200px")),
                          column(width=5,offset = 1,imageOutput('imagen10',height = "200px"))),
      #   conditionalPanel(condition = "input.monedas=='5'",column(width=4,imageOutput('imagen11',height = "200px")),
      #                    column(width=4,imageOutput('imagen12',height = "200px")),column(width=4,imageOutput('imagen13',height = "200px")),
      #                    column(width=4,offset = 2,imageOutput('imagen14',height = "200px")),column(width=5,imageOutput('imagen15',height = "200px"))),
      #   conditionalPanel(condition = "input.monedas=='6'",column(width=4,imageOutput('imagen16',height = "200px")),
      #                    column(width=4,imageOutput('imagen17',height = "200px")),column(width=4,imageOutput('imagen18',height = "200px")),
      #                    column(width=4,imageOutput('imagen19',height = "200px")),column(width=4,imageOutput('imagen20',height = "200px")),
      #                    column(width=4,imageOutput('imagen21',height = "200px"))),
      #   conditionalPanel(condition = "input.monedas=='7'",column(width=3,imageOutput('imagen22',height = "200px")),
      #                    column(width=3,imageOutput('imagen23',height = "200px")),column(width=3,imageOutput('imagen24',height = "200px")),
      #                    column(width=3,imageOutput('imagen25',height = "200px")),column(width=2,offset = 2,imageOutput('imagen26',height = "200px")),
      #                    column(width=3,offset=1,imageOutput('imagen27',height = "200px")),column(width=4,imageOutput('imagen28',height = "200px"))),
      #   conditionalPanel(condition = "input.monedas=='8'",column(width=3,imageOutput('imagen29',height = "200px")),
      #                    column(width=3,imageOutput('imagen30',height = "200px")),column(width=3,imageOutput('imagen31',height = "200px")),
      #                    column(width=3,imageOutput('imagen32',height = "200px")),column(width=3,imageOutput('imagen33',height = "200px")),
      #                    column(width=3,imageOutput('imagen34',height = "200px")),column(width=3,imageOutput('imagen35',height = "200px")),
      #                    column(width=3,imageOutput('imagen36',height = "200px"))),
      # conditionalPanel(condition = "input.monedas=='9'",column(width=2,imageOutput('imagen37',height = "200px")),
      #                  column(width=2,imageOutput('imagen38',height = "200px")),column(width=2,imageOutput('imagen39',height = "200px")),
      #                  column(width=2,imageOutput('imagen40',height = "200px")),column(width=2,imageOutput('imagen41',height = "200px")),
      #                  column(width=3,imageOutput('imagen42',height = "200px")),column(width=3,imageOutput('imagen43',height = "200px")),
      #                  column(width=3,imageOutput('imagen44',height = "200px")),column(width=3,imageOutput('imagen45',height = "200px"))),
      # conditionalPanel(condition = "input.monedas=='10'",column(width=2,imageOutput('imagen46',height = "200px")),
      #                  column(width=2,imageOutput('imagen47',height = "200px")),column(width=2,imageOutput('imagen48',height = "200px")),
      #                  column(width=2,imageOutput('imagen49',height = "200px")),column(width=2,imageOutput('imagen50',height = "200px")),
      #                  column(width=2,offset=1,imageOutput('imagen51',height = "200px")),column(width=2,imageOutput('imagen52',height = "200px")),
      #                  column(width=2,imageOutput('imagen53',height = "200px")),column(width=2,imageOutput('imagen54',height = "200px")),
      #                  column(width=2,imageOutput('imagen55',height = "200px")))),
      # 
      # 
      fluidRow(verbatimTextOutput('hola'))
    )
  )
)



server <- function(input, output,session) {
  prueba<-eventReactive(input$boton,{
    # rbinom(as.numeric(input$monedas),1,input$proba)
    sample(1:6,as.numeric(input$dados),replace = TRUE,prob = rep(1/6,6))
  })
  
  #1 lanzamiento
  output$imagen1<-renderImage({
      if(prueba()==1){
      list(src='www/img/dado1.png',height=200,width=200)
      } else if(prueba()==2){
      list(src='www/img/dado2.png',height=200,width=200)
      } else if(prueba()==3){
        list(src='www/img/dado3.png',height=200,width=200)
      } else if(prueba()==4){
        list(src='www/img/dado4.png',height=200,width=200)
      } else if(prueba()==5){
        list(src='www/img/dado5.png',height=200,width=200)
      } else if(prueba()==6){
        list(src='www/img/dado6.png',height=200,width=200)
      }
  },deleteFile = FALSE)
  
  # 2 lanzamiento
  output$imagen2<-renderImage({
    if(prueba()[1]==1){
      list(src='www/img/dado1.png',height=200,width=200)
    } else if(prueba()[1]==2){
      list(src='www/img/dado2.png',height=200,width=200)
    } else if(prueba()[1]==3){
      list(src='www/img/dado3.png',height=200,width=200)
    } else if(prueba()[1]==4){
      list(src='www/img/dado4.png',height=200,width=200)
    } else if(prueba()[1]==5){
      list(src='www/img/dado5.png',height=200,width=200)
    } else if(prueba()[1]==6){
      list(src='www/img/dado6.png',height=200,width=200)
    }
  },deleteFile = FALSE)

  output$imagen3<-renderImage({
    if(is.na(prueba()[2])==TRUE){
      list(src='')
    }
      else if(prueba()[2]==1){
      list(src='www/img/dado1.png',height=200,width=200)
    } else if(prueba()[2]==2){
      list(src='www/img/dado2.png',height=200,width=200)
    } else if(prueba()[2]==3){
      list(src='www/img/dado3.png',height=200,width=200)
    } else if(prueba()[2]==4){
      list(src='www/img/dado4.png',height=200,width=200)
    } else if(prueba()[2]==5){
      list(src='www/img/dado5.png',height=200,width=200)
    } else if(prueba()[2]==6){
      list(src='www/img/dado6.png',height=200,width=200)
    }
  },deleteFile = FALSE)

  # 3 lanzamientos
   output$imagen4<-renderImage({
     if(prueba()[1]==1){
       list(src='www/img/dado1.png',height=200,width=200)
     } else if(prueba()[1]==2){
       list(src='www/img/dado2.png',height=200,width=200)
     } else if(prueba()[1]==3){
       list(src='www/img/dado3.png',height=200,width=200)
     } else if(prueba()[1]==4){
       list(src='www/img/dado4.png',height=200,width=200)
     } else if(prueba()[1]==5){
       list(src='www/img/dado5.png',height=200,width=200)
     } else if(prueba()[1]==6){
       list(src='www/img/dado6.png',height=200,width=200)
     }
   },deleteFile = FALSE)
   
   
   output$imagen5<-renderImage({
     if(is.na(prueba()[2])==TRUE){
       list(src='')
     } 
       else if(prueba()[2]==1){
       list(src='www/img/dado1.png',height=200,width=200)
     } else if(prueba()[2]==2){
       list(src='www/img/dado2.png',height=200,width=200)
     } else if(prueba()[2]==3){
       list(src='www/img/dado3.png',height=200,width=200)
     } else if(prueba()[2]==4){
       list(src='www/img/dado4.png',height=200,width=200)
     } else if(prueba()[2]==5){
       list(src='www/img/dado5.png',height=200,width=200)
     } else if(prueba()[2]==6){
       list(src='www/img/dado6.png',height=200,width=200)
     } 
   },deleteFile = FALSE)
   
   output$imagen6<-renderImage({
        if(is.na(prueba()[3])==TRUE){
        list(src='')
      } 
       else if(prueba()[3]==1){
       list(src='www/img/dado1.png',height=200,width=200)
     } else if(prueba()[3]==2){
       list(src='www/img/dado2.png',height=200,width=200)
     } else if(prueba()[3]==3){
       list(src='www/img/dado3.png',height=200,width=200)
     } else if(prueba()[3]==4){
       list(src='www/img/dado4.png',height=200,width=200)
     } else if(prueba()[3]==5){
       list(src='www/img/dado5.png',height=200,width=200)
     } else if(prueba()[3]==6){
       list(src='www/img/dado6.png',height=200,width=200)
     } 
   },deleteFile = FALSE)
   
   #4 lanzamientos
   
   output$imagen7<-renderImage({
     if(prueba()[1]==1){
       list(src='www/img/dado1.png',height=150,width=150)
     } else if(prueba()[1]==2){
       list(src='www/img/dado2.png',height=150,width=150)
     } else if(prueba()[1]==3){
       list(src='www/img/dado3.png',height=150,width=150)
     } else if(prueba()[1]==4){
       list(src='www/img/dado4.png',height=150,width=150)
     } else if(prueba()[1]==5){
       list(src='www/img/dado5.png',height=150,width=150)
     } else if(prueba()[1]==6){
       list(src='www/img/dado6.png',height=150,width=150)
     }
   },deleteFile = FALSE)
   
   
   output$imagen8<-renderImage({
     if(is.na(prueba()[2])==TRUE){
       list(src='')
     } 
       else if(prueba()[2]==1){
       list(src='www/img/dado1.png',height=150,width=150)
     } else if(prueba()[2]==2){
       list(src='www/img/dado2.png',height=150,width=150)
     } else if(prueba()[2]==3){
       list(src='www/img/dado3.png',height=150,width=150)
     } else if(prueba()[2]==4){
       list(src='www/img/dado4.png',height=150,width=150)
     } else if(prueba()[2]==5){
       list(src='www/img/dado5.png',height=150,width=150)
     } else if(prueba()[2]==6){
       list(src='www/img/dado6.png',height=150,width=150)
     } 
   },deleteFile = FALSE)
   
   output$imagen9<-renderImage({
     if(is.na(prueba()[3])==TRUE){
       list(src='')
     } 
       else if(prueba()[3]==1){
       list(src='www/img/dado1.png',height=150,width=150)
     } else if(prueba()[3]==2){
       list(src='www/img/dado2.png',height=150,width=150)
     } else if(prueba()[3]==3){
       list(src='www/img/dado3.png',height=150,width=150)
     } else if(prueba()[3]==4){
       list(src='www/img/dado4.png',height=150,width=150)
     } else if(prueba()[3]==5){
       list(src='www/img/dado5.png',height=150,width=150)
     } else if(prueba()[3]==6){
       list(src='www/img/dado6.png',height=150,width=150)
     }
   },deleteFile = FALSE)
   
   output$imagen10<-renderImage({
     if(is.na(prueba()[4])==TRUE){
       list(src='')
     }
     
       else if(prueba()[4]==1){
       list(src='www/img/dado1.png',height=150,width=150)
     } else if(prueba()[4]==2){
       list(src='www/img/dado2.png',height=150,width=150)
     } else if(prueba()[4]==3){
       list(src='www/img/dado3.png',height=150,width=150)
     } else if(prueba()[4]==4){
       list(src='www/img/dado4.png',height=150,width=150)
     } else if(prueba()[4]==5){
       list(src='www/img/dado5.png',height=150,width=150)
     } else if(prueba()[4]==6){
       list(src='www/img/dado6.png',height=150,width=150)
     } 
   },deleteFile = FALSE)
  
    
  # # 5 lanzamientos
  # 
  # output$imagen11<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen12<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen13<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen14<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen15<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # # 6 lanzamiento
  # 
  # output$imagen16<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen17<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen18<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen19<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen20<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen21<-renderImage({
  #   if(prueba()[6]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[6]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # # 7 lanzamiento
  # 
  # output$imagen22<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen23<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen24<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen25<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen26<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen27<-renderImage({
  #   if(prueba()[6]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[6]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen28<-renderImage({
  #   if(prueba()[7]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[7]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # # 8 lanzamiento
  # 
  # output$imagen29<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen30<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen31<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen32<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen33<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen34<-renderImage({
  #   if(prueba()[6]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[6]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen35<-renderImage({
  #   if(prueba()[7]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[7]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen36<-renderImage({
  #   if(prueba()[8]==1){
  #     list(src='www/img/moneda1.jpg',height=150,width=150)
  #   } else if(prueba()[8]==0){
  #     list(src='www/img/moneda2.jpg',height=150,width=150)
  #   }
  # },deleteFile = FALSE)
  # 
  # # 9 lanzamiento
  # 
  # output$imagen37<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen38<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen39<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen40<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen41<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen42<-renderImage({
  #   if(prueba()[6]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[6]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen43<-renderImage({
  #   if(prueba()[7]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[7]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen44<-renderImage({
  #   if(prueba()[8]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[8]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen45<-renderImage({
  #   if(prueba()[9]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[9]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # # 10 lanzamiento
  # 
  # output$imagen46<-renderImage({
  #   if(prueba()[1]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[1]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen47<-renderImage({
  #   if(prueba()[2]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[2]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen48<-renderImage({
  #   if(prueba()[3]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[3]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen49<-renderImage({
  #   if(prueba()[4]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[4]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen50<-renderImage({
  #   if(prueba()[5]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[5]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen51<-renderImage({
  #   if(prueba()[6]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[6]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen52<-renderImage({
  #   if(prueba()[7]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[7]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen53<-renderImage({
  #   if(prueba()[8]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[8]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # 
  # output$imagen54<-renderImage({
  #   if(prueba()[9]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[9]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
  # output$imagen55<-renderImage({
  #   if(prueba()[10]==1){
  #     list(src='www/img/moneda1.jpg',height=125,width=125)
  #   } else if(prueba()[10]==0){
  #     list(src='www/img/moneda2.jpg',height=125,width=125)
  #   }
  # },deleteFile = FALSE)
  # 
   output$hola<-renderPrint({
     return(prueba())
   })
  }


shinyApp(ui = ui, server = server)
