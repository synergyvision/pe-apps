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
                                        numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-50,max=50,step=0.1,value = 1,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Función de Distribución'",
                                        numericInput(inputId = 'mu1',label = HTML('Seleccione el valor del parámetro &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'))
              ),
              conditionalPanel(condition = "input.nor=='Función de Densidad'",column(width=7,align='center',br(),verbatimTextOutput("norm"),plotOutput("densnor"))),
              conditionalPanel(condition = "input.nor=='Función de Distribución'",column(width=6,align='center',br(),verbatimTextOutput("norm1"),plotOutput("densnor1")))
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
    
    x <- seq(-50,50,0.01)
    hx <- dnorm(x,mean=media,sd=dv)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dnorm(x1,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  }


shinyApp(ui = ui, server = server)
