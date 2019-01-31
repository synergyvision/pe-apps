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
              tabPanel('Cálculos',br(),column(width=5,selectInput(inputId = 'nor',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Comparación de Medias','Comparación de Varianzas','Cuantiles','Muestra Aleatoria','Normal estándar'),selected = NULL),
                       conditionalPanel(condition = "input.nor=='Función de Densidad'",
                                        numericInput(inputId = 'mu',label = HTML('Seleccione el valor de la media &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-50,max=50,step=0.1,value = 5,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Función de Distribución'",
                                        numericInput(inputId = 'mu1',label = HTML('Seleccione el valor del parámetro &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma1',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor1',label = HTML('Seleccione el valor de la función de distribución'),min=-50,max=50,step=0.1,value = 5,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Comparación de Medias'",
                                        numericInput(inputId = 'mu_1',label = HTML('Seleccione el valor del parámetro &mu1;'),min=-10,max=10,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'mu_2',label = HTML('Seleccione el valor del parámetro &mu2;'),min=-10,max=10,step=0.1,value = 3,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Comparación de Varianzas'",
                                        numericInput(inputId = 'va_1',label = HTML('Seleccione el valor del parámetro &sigma1;'),min=-10,max=10,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'va_2',label = HTML('Seleccione el valor del parámetro &sigma2;'),min=-10,max=10,step=0.1,value = 3,width = '150px'))
                                        
              ),
              conditionalPanel(condition = "input.nor=='Función de Densidad'",column(width=7,align='center',br(),verbatimTextOutput("norm"),plotOutput("densnor"))),
              conditionalPanel(condition = "input.nor=='Función de Distribución'",column(width=6,align='center',br(),verbatimTextOutput("norm1"),plotOutput("densnor1"))),
              conditionalPanel(condition = "input.nor=='Comparación de Medias'",column(width=6,align='center',br(),plotOutput("densnor2"))),
              conditionalPanel(condition = "input.nor=='Comparación de Varianzas'",column(width=6,align='center',br(),plotOutput("densnor3")))
                       ))
)



server <- function(input, output,session) {
  
  output$norm<-renderText({
    x<-input$valor
    media<-input$mu
    dv<-input$sigma
    resultado<-paste("f(",x,") = ", dnorm(x,mean=media,sd=dv))
    return(resultado)
  })
  
  output$densnor<-renderPlot({
    x1<-input$valor
    
    media<-input$mu
    dv<-input$sigma
    
    x <- seq(media-6,6+media,0.01)
    hx <- dnorm(x,mean=media,sd=dv)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dnorm(x1,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  output$norm1<-renderText({
    x<-input$valor1
    media<-input$mu1
    dv<-input$sigma1
    
    resultado<-paste("F(",x,") = P(X <=",x,") = ", pnorm(x,mean = media, sd=dv))
    return(resultado)
  })
  
  
  output$densnor1<-renderPlot({
    x<-input$valor1
    media<-input$mu1
    dv<-input$sigma1
    
    data<-data.frame(norm=pnorm(seq(-x,x,0.01),mean = media,sd=dv))
    
    f1<-ggplot(data,aes(x=seq(-x,x,0.01),y=norm))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Normal",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  #revisar
  output$densnor2<-renderPlot({
  x<-seq(-15,15,0.01)
  y1 <- dnorm(x,mean=input$mu_1, sd=1)
  y2 <- dnorm(x,mean=input$mu_2, sd=1)
  
  dat<-data.frame(x,y1,y2)
  ggplot(data=dat, mapping = aes(x,y1))+geom_line(aes(colour = I("yellow")))+
    geom_area(mapping = aes(x,y1), fill = "yellow",alpha = .2)+
    geom_segment(aes(x = input$mu_1, y =0 , xend = input$mu_1,
                     yend = y1),linetype="dashed",colour="yellow")+
    geom_line(data=dat, aes(x,y2,colour = I("red")))+
    geom_segment(aes(x = input$mu_2, y =0 , xend = input$mu_2,
                     yend = y2),linetype="dashed",colour="red")+
    geom_area(mapping = aes(x,y2), fill = "red",alpha = .2)+
    labs(title = 'Medias de la distribución Normal',
         x = "x", y = "f(x)")+
    theme(plot.title = element_text(size = rel(1.3),hjust = 0.5))
  })
  
  output$densnor3<-renderPlot({
    x<-seq(-15,15,0.01)
    y1 <- dnorm(x,mean=0, sd=input$va_1)
    y2 <- dnorm(x,mean=0, sd=input$va_2)
    
    dat<-data.frame(x,y1,y2)
    ggplot(data=dat, mapping = aes(x,y1))+geom_line(aes(colour = I("yellow")))+
      geom_area(mapping = aes(x,y1), fill = "yellow",alpha = .2)+
      # geom_segment(aes(x = input$mu_1, y =0 , xend = input$mu_1,
      #                  yend = y1),linetype="dashed",colour="yellow")+
      geom_line(data=dat, aes(x,y2,colour = I("red")))+
      # geom_segment(aes(x = input$mu_2, y =0 , xend = input$mu_2,
      #                  yend = y2),linetype="dashed",colour="red")+
      geom_area(mapping = aes(x,y2), fill = "red",alpha = .2)+
      labs(title = 'Varianzas de la distribución Normal',
           x = "x", y = "f(x)")+
      theme(plot.title = element_text(size = rel(1.3),hjust = 0.5)) 
  })
  
  
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
