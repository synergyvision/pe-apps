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
                                                                                                  numericInput(inputId = 'valor1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=1,step=1,value = 1,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Cuantiles'",
                                                                                                  numericInput(inputId = 'proba2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Muestra Aleatoria'",
                                                                                                  numericInput(inputId = 'proba3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'))
                                                                                 ), 
                                                                                 conditionalPanel(condition = "input.ber=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli"),plotOutput("dens1"))),
                                                                                 conditionalPanel(condition = "input.ber=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli1"),plotOutput("dist1"))),
                                                                                 conditionalPanel(condition = "input.ber=='Cuantiles'",column(align='center',width=6,br(),br(),br(),br(),br(),br(),verbatimTextOutput("bernoulli2"))),
                                                                                 conditionalPanel(condition = "input.ber=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli3"),plotOutput("dens2")))
                                                                                 ))),
      conditionalPanel(condition = "input.distribucion=='Binomial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características",includeMarkdown("binomial.Rmd")),
                                                                                tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'bin',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                conditionalPanel(condition = "input.bin=='Función de Densidad'",
                                                                                                 numericInput(inputId = 'probabin',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Función de Distribución'",
                                                                                                 numericInput(inputId = 'probabin1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin1',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Cuantiles'",
                                                                                                 numericInput(inputId = 'probabin2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin2',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Muestra Aleatoria'",
                                                                                                 numericInput(inputId = 'probabin3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin3',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px')
                                                                                                 )
                                                                                ),
                                                                                conditionalPanel(condition = "input.bin=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("binomial"),plotOutput("densbin"))),
                                                                                conditionalPanel(condition = "input.bin=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("binomial1"),plotOutput("densbin1"))),
                                                                                conditionalPanel(condition = "input.bin=='Cuantiles'",column(align='center',width=6,br(),br(),br(),br(),br(),br(),verbatimTextOutput("binomial2"))),
                                                                                conditionalPanel(condition = "input.bin=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("binomial3"),plotOutput("densbin2")))
                                                                                ))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características",includeMarkdown("geometrica.Rmd")),
                                                                                  tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'geo',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                  conditionalPanel(condition = "input.geo=='Función de Densidad'",
                                                                                                                    numericInput(inputId = 'probageo',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorgeo',label = HTML('Elija la cantidad de ensayos para obtener el primer éxito <i>n</i>'),min=1,max=100,step=1,value = 1,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Función de Distribución'",
                                                                                                                    numericInput(inputId = 'probageo1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorgeo1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=1,max=100,step=1,value = 1,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Cuantiles'",
                                                                                                                    numericInput(inputId = 'probageo2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorgeo2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Muestra Aleatoria'",
                                                                                                                    numericInput(inputId = 'probageo3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorgeo3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'))
                                                                                  ),
                                                                                  conditionalPanel(condition = "input.geo=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("geometrica"),plotOutput("densgeo"))),
                                                                                  conditionalPanel(condition = "input.geo=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("geometrica1"),plotOutput("densgeo1"))),
                                                                                  conditionalPanel(condition = "input.geo=='Cuantiles'",column(align='center',width=6,br(),br(),br(),br(),br(),br(),verbatimTextOutput("geometrica2"))),
                                                                                  conditionalPanel(condition = "input.geo=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("geometrica3"),plotOutput("densgeo2")))
                                                                                  ))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características",includeMarkdown("hipergeometrica.Rmd")),
                                                                                       tabPanel('Cálculos',br(),br(),selectInput(inputId = 'hip',label = HTML('Seleccione la distribución deseada'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL)
                                                                                  ))),
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
    if(input$valor==1){
    data<-data.frame(x=c(input$valor,1-input$valor),pro1=c(input$proba,1-input$proba))
    } else{
      data<-data.frame(x=c(input$valor,1-input$valor),pro1=c(1-input$proba,input$proba))
    }
    f<-ggplot(data, mapping = aes(x,pro1))+geom_point(colour="blue",size=5)+
      labs( title = "Densidad Bernoulli",
            x = "x", y = "f(x)", caption = "http://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f)
  })
  
  output$bernoulli1<-renderText({
    n1<-input$valor1
    p1<-input$proba1
    resultado1<-paste("F(",n1,") = P(X <=",n1,") = ", pbinom(n1,1,p1))
    return(resultado1)
  })
  
  output$dist1<-renderPlot({
    if(input$valor1==1){
      data1<-data.frame(x=c(input$valor1,1-input$valor1),pro1=c(input$proba1,1-input$proba1))
    } else{
      data1<-data.frame(x=c(input$valor1,1-input$valor1),pro1=c(1-input$proba1,input$proba1))
    }
    
    f1<-ggplot(data1, aes(x=data1[,1],y=data1[,2]))+
        geom_segment(aes(x=0,y=1-input$proba1,xend=1,yend=1-input$proba1),size=1,color="blue")+geom_segment(aes(x=1,y=1,xend=1.3,yend=1),size=1,color="blue")+geom_segment(aes(x=1,y=1-input$proba1,xend=1,yend=1),size=1,color="blue")+
        labs( title = "Distribución Bernoulli",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f1)
  })
  
  output$bernoulli2<-renderText({
    #qbinom(p=input$valor2,size = 1,prob = input$proba2,lower.tail = TRUE)
    w<-paste("x = ", qbinom(p=input$valor2,size = 1,prob = input$proba2,lower.tail = TRUE))
    return(w)
  })
  
  muestraber<-reactive({
    rbinom(n=input$valor3,size=1,prob=input$proba3)
  })
  
  output$bernoulli3<-renderPrint({
    return(muestraber())
  })
  
  output$dens2<-renderPlot({
    data2<-data.frame(x1=muestraber())
    f2<-ggplot(data2,mapping=aes(x=1:length(x1),y=x1))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x1))+
      labs( title = "Muestra aleatoria",
           x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$binomial<-renderText({
    x<-input$valorbin
    n<-input$ensayobin
    p<-input$probabin
    resultado2<-paste("f(",x,") = P(X =",x,") = ", dbinom(x,n,p))
    return(resultado2)
  })
  
  output$densbin<-renderPlot({
    data3<-data.frame(bin=dbinom(0:input$valorbin,input$ensayobin,input$probabin))
    f3<-ggplot(data3,aes(x=0:(length(bin)-1),y=bin))+geom_point(colour='blue',size=2)+scale_x_continuous(breaks = 0:(length(data3$bin)-1))+
      labs( title = "Densidad Binomial",
            x = "x", y = "f(x)", caption = "http://synergy.vision/" )
    return(f3)
  })
  
  output$binomial1<-renderText({
    x<-input$valorbin1
    n<-input$ensayobin1
    p<-input$probabin1
    resultado3<-paste("F(",x,") = P(X <=",x,") = ", pbinom(x,n,p,lower.tail = T))
    return(resultado3)
  })
  
  output$densbin1<-renderPlot({
    data3<-data.frame(bin=pbinom(0:input$valorbin1,input$ensayobin1,input$probabin1))
    f4<-ggplot(data3,aes(x=0:(length(bin)-1),y=bin))+geom_step(colour='blue',size=1)+scale_x_continuous(breaks = 0:length(data3$bin)-1)+
      labs( title = "Distribución Binomial",
            x = "x", y = "f(x)", caption = "http://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f4)
  })
  
  output$binomial2<-renderText({
    #qbinom(p=input$valor2,size = 1,prob = input$proba2,lower.tail = TRUE)
    w<-paste("x = ", qbinom(p=input$valorbin2,size = input$ensayobin2,prob = input$probabin2,lower.tail = TRUE))
    return(w)
  })
  
  muestraber1<-reactive({
    rbinom(n=input$valorbin3,size=input$ensayobin3,prob=input$probabin3)
  })
  
  output$binomial3<-renderPrint({
    return(muestraber1())
  })
  
  output$densbin2<-renderPlot({
    data5<-data.frame(x1=muestraber1())
    f5<-ggplot(data5,mapping=aes(x=1:length(x1),y=x1))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data5$x1))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f5)
  })
  
  output$geometrica<-renderText({
    p<-input$probageo
    x<-input$valorgeo
    resultado4<-paste("f(",x,") = P(X =",x,") = ", dgeom(x-1,p))
    return(resultado4)
  })
  
  output$densgeo<-renderPlot({
    data5<-data.frame(geom=dgeom(0:(input$valorgeo-1),input$probageo))
    f5<-ggplot(data5,aes(x=1:length(geom),y=geom))+geom_point(colour='blue',size=2)+scale_x_continuous(breaks = 1:length(data5$geom))+
      labs( title = "Densidad geometrica",
            x = "x", y = "f(x)", caption = "http://synergy.vision/" )
    return(f5)
  })
  
  output$geometrica1<-renderText({
    x<-input$valorgeo1
    p<-input$probageo1
    resultado5<-paste("F(",x,") = P(X <=",x,") = ", pgeom(x-1,p,lower.tail = T))
    return(resultado5)
  })
  
  output$densgeo1<-renderPlot({
    data6<-data.frame(geom=pgeom(0:(input$valorgeo1-1),input$probageo1))
    f6<-ggplot(data6,aes(x=1:length(geom),y=geom))+geom_step(colour='blue',size=1)+scale_x_continuous(breaks = 1:length(data6$geom))+
      labs( title = "Distribución Geométrica",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f6)
  })
  
  output$geometrica2<-renderText({
    
    w<-paste("x = ", qgeom(p=input$valorgeo2,prob = input$probageo2,lower.tail = TRUE)+1)
    return(w)
  })
  
  muestraber3<-reactive({
    rgeom(n=input$valorgeo3,prob=input$probageo3)+1
  })
  
  output$geometrica3<-renderPrint({
    return(muestraber3())
  })
  
  output$densgeo2<-renderPlot({
    data7<-data.frame(x1=muestraber3())
    f7<-ggplot(data7,mapping=aes(x=1:length(x1),y=x1))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data7$x1))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f7)
  })
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
