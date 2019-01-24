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
ensure_version("knitr","1.21")
ensure_version("latex2exp","0.4.0")


library(knitr)
library(latex2exp)
library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
#library(modeest)
library(matrixStats)
library(ggplot2)


ui <- fluidPage(

  titlePanel("Distribuciones Continuas"),
  sidebarLayout(

    sidebarPanel(

      selectInput(inputId = 'distribucion',label = HTML('Seleccione la distribución deseada'),choices = c('Uniforme','Exponencial','Gamma','Beta','Chi-cuadrado','Fisher-Snedecor','t-Student','Weibull','Cauchy'),selected = NULL)
      ),

    mainPanel(

      withMathJax(),
      conditionalPanel(condition = "input.distribucion=='Uniforme'",tabsetPanel(type = "pills", id="pri",tabPanel('Características',includeHTML("uniforme.html")),
                                                                                 tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'uni',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                          conditionalPanel(condition = "input.uni=='Función de Densidad'",
                                                                                                           sliderInput('rangouni',label = 'Parámetros [a,b]',min = -40,max=40,value=c(0,1),step = 1,width = '300px'),
                                                                                                           numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-40,max=40,step=1,value = 1,width = '150px'))
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.uni=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("unif"),plotOutput("dens1")))
                                                                                          ))),
      conditionalPanel(condition = "input.distribucion=='Exponencial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características",includeHTML("exponencial.html")),
                                                                                tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Gamma'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características",includeHTML("gamma.html")),
                                                                                  tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Beta'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características",includeHTML("beta.html")),
                                                                                       tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Chi-cuadrado'",tabsetPanel(type = "pills", id="pri5",tabPanel("Características",includeHTML('Chi-cuadrado.html')),
                                                                                   tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Fisher-Snedecor'",tabsetPanel(type = "pills", id="pri6",tabPanel("Características",includeHTML('Fisher-snedcor.html')),
                                                                               tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='t-Student'",tabsetPanel(type = "pills", id="pri7",tabPanel("Características",includeHTML('t-Student.html')),
                                                                                         tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Weibull'",tabsetPanel(type = "pills", id="pri8",tabPanel("Características",includeHTML('Weibull.html')),
                                                                                         tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Cauchy'",tabsetPanel(type = "pills", id="pri9",tabPanel("Características",includeHTML('Cauchy.html')),
                                                                              tabPanel('Cálculos',br(),br())))
    )
  )
)



server <- function(input, output,session) {
  
  output$unif<-renderText({
    x<-input$valor
    l<-input$rangouni[1]
    u<-input$rangouni[2]
    resultado<-paste("f(",x,") = ", dunif(x,l,u))
    return(resultado)
  })
  
  output$dens1<-renderPlot({
    x1<-input$valor
    l<-input$rangouni[1]
    u<-input$rangouni[2]
    
    x <- seq(-40,40,0.01)
    hx <- dunif(x,l,u)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x= ifelse(x >= l & x <= u,x,x)), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dunif(x1,l,u)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad uniforme',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })

  }


shinyApp(ui = ui, server = server)
