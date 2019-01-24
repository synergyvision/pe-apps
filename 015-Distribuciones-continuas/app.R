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
                                                                                                           numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-40,max=40,step=1,value = 1,width = '150px')),
                                                                                          conditionalPanel(condition = "input.uni=='Función de Distribución'",
                                                                                                           sliderInput('rangouni1',label = 'Parámetros [a,b]',min = -40,max=40,value=c(0,1),step = 1,width = '300px'),
                                                                                                           numericInput(inputId = 'valor1',label = HTML('Seleccione el valor de la función de distribución'),min=-40,max=40,step=1,value = 1,width = '150px')),
                                                                                          conditionalPanel(condition = "input.uni=='Cuantiles'",
                                                                                                           sliderInput('rangouni2',label = 'Parámetros [a,b]',min = -40,max=40,value=c(0,1),step = 1,width = '300px'),
                                                                                                           numericInput(inputId = 'valor2',label = HTML('Seleccione la probabilidad del cuantil deseado'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                          conditionalPanel(condition = "input.uni=='Muestra Aleatoria'",
                                                                                                           sliderInput('rangouni3',label = 'Parámetros [a,b]',min = -40,max=40,value=c(0,1),step = 1,width = '300px'),
                                                                                                           numericInput(inputId = 'valor3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=0,max=100,step=1,value = 10,width = '150px'))
                                                                                          ),
                                                                                          conditionalPanel(condition = "input.uni=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("unif"),plotOutput("dens1"))),
                                                                                          conditionalPanel(condition = "input.uni=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("unif1"),plotOutput("dens2"))),
                                                                                          conditionalPanel(condition = "input.uni=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("unif2"))),
                                                                                          conditionalPanel(condition = "input.uni=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("unif3"),plotOutput("dens3")))
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

  output$unif1<-renderText({
    x<-input$valor1
    l<-input$rangouni1[1]
    u<-input$rangouni1[2]
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", punif(x,l,u))
    return(resultado1)
  })
  
  output$dens2<-renderPlot({
    x<-input$valor1
    l<-input$rangouni1[1]
    u<-input$rangouni1[2]
    data1<-data.frame(uni=punif(l:x,l,u,lower.tail = TRUE))
    
    f1<-ggplot(data1,aes(x=l:x,y=uni))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Uniforme",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  output$unif2<-renderText({
    x<-input$valor2
    l<-input$rangouni2[1]
    u<-input$rangouni2[2]
    resultado2<-paste("x = ", qunif(x,l,u))
    return(resultado2)
  })
  
  muestra<-reactive({
    x<-input$valor3
    l<-input$rangouni3[1]
    u<-input$rangouni3[2]
    
    runif(x,l,u)
  })
  
  output$unif3<-renderPrint({
    return(muestra())
  })
  
  output$dens3<-renderPlot({
    data2<-data.frame(x=muestra())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  
  
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
