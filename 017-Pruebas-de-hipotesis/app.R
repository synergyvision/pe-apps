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
  sidebarLayout(
    
    sidebarPanel(
  
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
    ),
  
  mainPanel(
    conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza conocida'",column(width=5,numericInput(inputId = 'MediaHip',label = 'Inserte Media hipotética',min=0,max = 100,value = 0,step = 0.1,width = '150px'),
                     numericInput(inputId = 'MediaMuestral',label = 'Inserte Media de la muestra',min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                     numericInput(inputId = 'Muestra',label = 'Inserte Tamaño de la muestra',min=0,max = 100,value = 5,step = 1,width = '150px'),
                     numericInput(inputId = 'VarianzaPob',label = 'Inserte Varianza Poblacional',min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
                     numericInput(inputId = 'signif',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),column(width = 7,plotOutput('grafica1'))
                     ),
    conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza desconocida'",column(width=5,numericInput(inputId = 'MediaHip1',label = 'Inserte Media hipotética',min=0,max = 100,value = 0,step = 0.1,width = '150px'),
                                                                                                                 numericInput(inputId = 'MediaMuestral1',label = 'Inserte Media de la muestra',min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                                                                                                                 numericInput(inputId = 'Muestra1',label = 'Inserte Tamaño de la muestra',min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                      
                                                                                                                numericInput(inputId = 'signif1',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),column(width = 7,plotOutput('grafica2')))
    
    
    )
  )
)



server <- function(input, output,session) {
  
  output$grafica1<-renderPlot({
    mu<-input$MediaHip
    x_bar<-input$MediaMuestral
    n<-input$Muestra
    sigma<-sqrt(input$VarianzaPob)
    alpha<-input$signif
    
    z<-(x_bar-mu)*sqrt(n)/sigma
    
    z_alpha<-if(z<0){
      qnorm(alpha,mean=0,sd=1)
    }
    else{
      qnorm(1-alpha,mean=0,sd=1)
    }
    
    x<-seq(-4,4,0.01)
    y<-dnorm(x,mean=0, sd=1)
    f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
      geom_area(mapping = aes(x,y), fill = "blue",alpha = .2)+
      
      geom_segment(aes(x = z_alpha, y =0 , xend = z_alpha, yend = dnorm(z_alpha,mean=0, sd=1)), colour = "black",linetype=2)+
      geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+
      
      annotate("text", x=z, y =-0.02, label ="z", parse = TRUE)+
      annotate("text", x=z_alpha, y =-0.02, label="'Z'[alpha]", parse = TRUE)+
      annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
      annotate("text", x=z_alpha+0.5, y=0.05, label="'Rechazar H'[0]", parse = TRUE)+
      
      ylim(-0.05,0.41)+
      xlim(-4,4)+
      labs( title = "Prueba de cola derecha Distribución Normal",
            x = " ", y = " ",caption = "http://synergy.vision/" )
    
    return(f)
  })
  
  
  
  # output$grafica2<-renderPlot({
  #   mu<-input$MediaHip1
  #   x_bar<-input$MediaMuestral1
  #   n<-input$Muestra1
  #   alpha<-input$signif1
  #   
  #   t<-(x_bar-mu)/(s/sqrt(n))
  #   
  #   z_alpha<-if(z<0){
  #     qnorm(alpha,mean=0,sd=1)
  #   }
  #   else{
  #     qnorm(1-alpha,mean=0,sd=1)
  #   }
  #   
  #   x<-seq(-4,4,0.01)
  #   y<-dnorm(x,mean=0, sd=1)
  #   f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
  #     geom_area(mapping = aes(x,y), fill = "blue",alpha = .2)+
  #     
  #     geom_segment(aes(x = z_alpha, y =0 , xend = z_alpha, yend = dnorm(z_alpha,mean=0, sd=1)), colour = "black",linetype=2)+
  #     geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+
  #     
  #     annotate("text", x=z, y =-0.02, label ="z", parse = TRUE)+
  #     annotate("text", x=z_alpha, y =-0.02, label="'Z'[alpha]", parse = TRUE)+
  #     annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
  #     annotate("text", x=z_alpha+0.5, y=0.05, label="'Rechazar H'[0]", parse = TRUE)+
  #     
  #     ylim(-0.05,0.41)+
  #     xlim(-4,4)+
  #     labs( title = "Prueba de cola derecha Distribución Normal",
  #           x = " ", y = " ",caption = "http://synergy.vision/" )
  #   
  #   return(f)
  # })
  # 
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
