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
  
  titlePanel("Distribuciones discretas"),
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = 'distribucion',label = HTML('Seleccione la distribución deseada'),choices = c('Bernoulli','Binomial','Geométrica','Hipergeométrica','Multinomial','Poisson','Binomial negativa'),selected = NULL)
      ),

    mainPanel(

      withMathJax(),
      conditionalPanel(condition = "input.distribucion=='Bernoulli'",tabsetPanel(type = "pills", id="pri",tabPanel('Características',includeMarkdown("bernoulli.Rmd")),
                                                                                 tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'ber',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                 conditionalPanel(condition = "input.ber=='Función de Densidad'",
                                                                                                  numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=1,step=1,value = 1,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Función de Distribución'",
                                                                                                  numericInput(inputId = 'proba1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=1,step=1,value = 1,width = '150px'))
                                                                                 ),
                                                                                 conditionalPanel(condition = "input.ber=='Función de Densidad'",column(width=7,br(),verbatimTextOutput("bernoulli"),plotOutput("dens1"))),
                                                                                 conditionalPanel(condition = "input.ber=='Función de Distribución'",br(),column(width=7,verbatimTextOutput("bernoulli1")))
                                                                                 ))),
      conditionalPanel(condition = "input.distribucion=='Binomial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características",includeMarkdown("binomial.Rmd")),
                                                                                tabPanel('Cálculos',br(),br(),selectInput(inputId = 'bin',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)
                                                                                ))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características",includeMarkdown("geometrica.Rmd")),
                                                                                  tabPanel('Cálculos',br(),br(),selectInput(inputId = 'geo',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características",includeMarkdown("hipergeometrica.Rmd")),
                                                                                       tabPanel('Cálculos',br(),br(),selectInput(inputId = 'hip',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)))),
      conditionalPanel(condition = "input.distribucion=='Multinomial'",tabsetPanel(type = "pills", id="pri5",tabPanel("Características",includeMarkdown("multinomial.Rmd")),
                                                                                   tabPanel('Cálculos',br(),br(),selectInput(inputId = 'mult',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)))),
      conditionalPanel(condition = "input.distribucion=='Poisson'",tabsetPanel(type = "pills", id="pri6",tabPanel("Características",includeMarkdown("poisson.Rmd")),
                                                                               tabPanel('Cálculos',br(),br(),selectInput(inputId = 'poi',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)))),
      conditionalPanel(condition = "input.distribucion=='Binomial negativa'",tabsetPanel(type = "pills", id="pri7",tabPanel("Características",includeMarkdown("binonegativa.Rmd")),
                                                                                         tabPanel('Cálculos',br(),br(),selectInput(inputId = 'binega',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL))))
    )
  )
)



server <- function(input, output,session) {
  
  output$bernoulli<-renderText({
    n<-input$valor
    p<-input$proba
    resultado<-paste("f(",n,") = P(X =",n,") = ", dbinom(n,1,p))
    return(resultado)
  })
  
  output$dens1<-renderPlot({
    data<-data.frame(x=c(input$valor,1-input$valor),pro1=c(input$proba,1-input$proba))
    f<-ggplot(data, mapping = aes(x,pro1))+geom_point(colour="blue",size=5)+
      labs( title = "Densidad Bernoulli",
            x = "x", y = "f(x)", caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$bernoulli1<-renderText({
    n1<-input$valor1
    p1<-input$proba1
    resultado1<-paste("F(",n1,") = P(X <=",n1,") = ", pbinom(n1,1,p1))
    return(resultado1)
  })
  
  }


shinyApp(ui = ui, server = server)
