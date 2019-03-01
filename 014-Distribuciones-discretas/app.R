ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.2.0")
ensure_version("readxl", "1.2.0")
ensure_version("shinydashboard", "0.7.1")
ensure_version("psych", "1.8.10")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.1.0")




library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
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
                                                                                 tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'ber',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                                 conditionalPanel(condition = "input.ber=='Función de densidad'",
                                                                                                  numericInput(inputId = 'proba',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=1,step=1,value = 1,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Función de distribución'",
                                                                                                  numericInput(inputId = 'proba1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=1,step=1,value = 1,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Cuantiles'",
                                                                                                  numericInput(inputId = 'proba2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                 conditionalPanel(condition = "input.ber=='Muestra aleatoria'",
                                                                                                  numericInput(inputId = 'proba3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                  numericInput(inputId = 'valor3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'))
                                                                                 ),
                                                                                 conditionalPanel(condition = "input.ber=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli"),plotOutput("dens1"))),
                                                                                 conditionalPanel(condition = "input.ber=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli1"),plotOutput("dist1"))),
                                                                                 conditionalPanel(condition = "input.ber=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli2"),plotOutput("dens3"))),
                                                                                 conditionalPanel(condition = "input.ber=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("bernoulli3"),plotOutput("dens2")))
                                                                                 ))),
      conditionalPanel(condition = "input.distribucion=='Binomial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características",includeMarkdown("binomial.Rmd")),
                                                                                tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'bin',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                                conditionalPanel(condition = "input.bin=='Función de densidad'",
                                                                                                 numericInput(inputId = 'probabin',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Función de distribución'",
                                                                                                 numericInput(inputId = 'probabin1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin1',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Cuantiles'",
                                                                                                 numericInput(inputId = 'probabin2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin2',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 1,width = '150px')),
                                                                                conditionalPanel(condition = "input.bin=='Muestra aleatoria'",
                                                                                                 numericInput(inputId = 'probabin3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                 numericInput(inputId = 'valorbin3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'),
                                                                                                 numericInput(inputId = 'ensayobin3',label = HTML('Elija la cantidad de ensayos <i>n</i>'),min=0,max=100,step=1,value = 1,width = '150px')
                                                                                                 )
                                                                                ),
                                                                                conditionalPanel(condition = "input.bin=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("binomial"),plotOutput("densbin"))),
                                                                                conditionalPanel(condition = "input.bin=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("binomial1"),plotOutput("densbin1"))),
                                                                                conditionalPanel(condition = "input.bin=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("binomial2"),plotOutput("densbin3"))),
                                                                                conditionalPanel(condition = "input.bin=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("binomial3"),plotOutput("densbin2")))
                                                                                ))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características",includeMarkdown("geometrica.Rmd")),
                                                                                  tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'geo',label = HTML('Seleccione el cáculo deseado'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                                  conditionalPanel(condition = "input.geo=='Función de densidad'",
                                                                                                   numericInput(inputId = 'probageo',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                   numericInput(inputId = 'valorgeo',label = HTML('Elija la cantidad de ensayos para obtener el primer éxito <i>n</i>'),min=1,max=100,step=1,value = 1,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Función de distribución'",
                                                                                                   numericInput(inputId = 'probageo1',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                   numericInput(inputId = 'valorgeo1',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=1,max=100,step=1,value = 1,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Cuantiles'",
                                                                                                   numericInput(inputId = 'probageo2',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                   numericInput(inputId = 'valorgeo2',label = HTML('Inserte probabilidad &alpha; para el cálculo del <br/> cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                  conditionalPanel(condition = "input.geo=='Muestra aleatoria'",
                                                                                                   numericInput(inputId = 'probageo3',label=HTML('Elija la probabilidad <br/>de éxito'),value = 0.5,min = 0,max = 1,step = 0.1,width = '150px'),
                                                                                                   numericInput(inputId = 'valorgeo3',label = HTML('Inserte el número de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'))
                                                                                  ),
                                                                                  conditionalPanel(condition = "input.geo=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("geometrica"),plotOutput("densgeo"))),
                                                                                  conditionalPanel(condition = "input.geo=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("geometrica1"),plotOutput("densgeo1"))),
                                                                                  conditionalPanel(condition = "input.geo=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("geometrica2"),plotOutput("densgeo3"))),
                                                                                  conditionalPanel(condition = "input.geo=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("geometrica3"),plotOutput("densgeo2")))
                                                                                  ))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características",includeMarkdown("hipergeometrica.Rmd")),
                                                                                       tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'hip',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                                       conditionalPanel(condition = "input.hip=='Función de densidad'",
                                                                                                        numericInput(inputId = 'valorhip',label = HTML('Tamaño de la muestra'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip2',label = HTML('Elementos con característica <i>I</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip3',label = HTML('Elementos con característica <i>II</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip4',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 3,width = '150px')),
                                                                                       conditionalPanel(condition = "input.hip=='Función de distribución'",
                                                                                                        numericInput(inputId = 'valorhip5',label = HTML('Tamaño de la muestra'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip6',label = HTML('Elementos con característica <i>I</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip7',label = HTML('Elementos con característica <i>II</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip8',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 2,width = '150px')),
                                                                                       conditionalPanel(condition = "input.hip=='Cuantiles'",
                                                                                                        numericInput(inputId = 'valorhip9',label = HTML('Tamaño de la muestra'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip10',label = HTML('Elementos con característica <i>I</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip11',label = HTML('Elementos con característica <i>II</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip12',label = HTML('Seleccione la probabilidad que le corresponde al cuantil'),min=0,max=1,step=0.1,value = 0.6,width = '150px')),
                                                                                       conditionalPanel(condition = "input.hip=='Muestra aleatoria'",
                                                                                                        numericInput(inputId = 'valorhip13',label = HTML('Tamaño de la muestra'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip14',label = HTML('Elementos con característica <i>I</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip15',label = HTML('Elementos con característica <i>II</i>'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                        numericInput(inputId = 'valorhip16',label = HTML('Seleccione el número de muestra deseado'),min=1,max=100,step=1,value = 30,width = '150px'))
                                                                                       ),
                                                                                       conditionalPanel(condition = "input.hip=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("hiper"),plotOutput("denship"))),
                                                                                       conditionalPanel(condition = "input.hip=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("hiper1"),plotOutput("denship1"))),
                                                                                       conditionalPanel(condition = "input.hip=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("hiper2"),plotOutput("denship3"))),
                                                                                       conditionalPanel(condition = "input.hip=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("hiper3"),plotOutput("denship2")))
                                                                                  ))),
      conditionalPanel(condition = "input.distribucion=='Multinomial'",tabsetPanel(type = "pills", id="pri5",tabPanel("Características",includeMarkdown("multinomial.Rmd")),
                                                                                   tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'mult',label = HTML('Seleccione la distribución deseada'),choices = c('Función de densidad','Muestra aleatoria'),selected = NULL),
                                                                                                                        conditionalPanel(condition = "input.mult=='Función de densidad'",
                                                                                                                                         textInput(inputId = "vector", label = "Introducir el vector de probabilidades",placeholder = "0.1,0.2,...",value = '0.2,0.3'),
                                                                                                                                         textInput(inputId = "vector2", label = "Introducir los valores al cual se le quiere calcular la probabilidad",placeholder = "0,1,2,...",value = '5,4')),
                                                                                                                        conditionalPanel(condition = "input.mult=='Muestra aleatoria'",
                                                                                                                                         textInput(inputId = "vector3", label = "Introducir el vector de probabilidades",placeholder = "0.1,0.2,...",value = '0.2,0.3'),
                                                                                                                                         numericInput(inputId = "vector5", label = "Introducir el número total de la población",min=1,max=200,step = 1,value = 1,width = '150px'),
                                                                                                                                         numericInput(inputId = "vector4", label = "Introducir el número de muestra deseado",min=1,max=200,step = 1,value = 1,width = '150px'))

                                                                                   ),
                                                                                   conditionalPanel(condition = "input.mult=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("multin"),plotOutput("densmulti"))),
                                                                                   conditionalPanel(condition = "input.mult=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("multin1")))
                                                                                            ))),
      conditionalPanel(condition = "input.distribucion=='Poisson'",tabsetPanel(type = "pills", id="pri6",tabPanel("Características",includeMarkdown("poisson.Rmd")),
                                                                               tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'poi',label = HTML('Seleccione la distribución deseada'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                               conditionalPanel(condition = "input.poi=='Función de densidad'",
                                                                                                numericInput(inputId = 'valorpoi',label = HTML('Inserte el parámetro &lambda;'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorpoi2',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                               conditionalPanel(condition = "input.poi=='Función de distribución'",
                                                                                                numericInput(inputId = 'valorpoi3',label = HTML('Inserte el parámetro &lambda;'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorpoi4',label = HTML('Seleccione el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 1,width = '150px')),
                                                                               conditionalPanel(condition = "input.poi=='Cuantiles'",
                                                                                                numericInput(inputId = 'valorpoi5',label = HTML('Inserte el parámetro &lambda;'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorpoi6',label = HTML('Seleccione el valor de la probabilidad que le corresponde al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                               conditionalPanel(condition = "input.poi=='Muestra aleatoria'",
                                                                                                numericInput(inputId = 'valorpoi7',label = HTML('Inserte el parámetro &lambda;'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorpoi8',label = HTML('Seleccione el número de muestra deseado'),min=0,max=200,step=1,value = 5,width = '150px'))
                                                                               ),
                                                                               conditionalPanel(condition = "input.poi=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("dispoison"),plotOutput("denspoi"))),
                                                                               conditionalPanel(condition = "input.poi=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("dispoison1"),plotOutput("denspoi1"))),
                                                                               conditionalPanel(condition = "input.poi=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("dispoison2"),plotOutput("denspoi3"))),
                                                                               conditionalPanel(condition = "input.poi=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("dispoison3"),plotOutput("denspoi2")))
                                                                                        ))),
      conditionalPanel(condition = "input.distribucion=='Binomial negativa'",tabsetPanel(type = "pills", id="pri7",tabPanel("Características",includeMarkdown("binonegativa.Rmd")),
                                                                                         tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'binega',label = HTML('Seleccione la distribución deseada'),choices = c('Función de densidad','Función de distribución','Cuantiles','Muestra aleatoria'),selected = NULL),
                                                                                                  conditionalPanel(condition = "input.binega=='Función de densidad'",
                                                                                                                   numericInput(inputId = 'valorbine',label = HTML('Inserte el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 10,width = '150px'),
                                                                                                                   numericInput(inputId = 'valorbine1',label = HTML('Seleccione la cantidad de éxito'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                                   numericInput(inputId = 'valorbine2',label = HTML('Inserte la probabilidad de que ocurra un éxito'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                   conditionalPanel(condition = "input.binega=='Función de distribución'",
                                                                                                                    numericInput(inputId = 'valorbine3',label = HTML('Inserte el valor al cual se le quiere calcular la probabilidad'),min=0,max=100,step=1,value = 10,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine4',label = HTML('Seleccione la cantidad de éxito'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine5',label = HTML('Inserte la probabilidad de que ocurra un éxito'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                   conditionalPanel(condition = "input.binega=='Cuantiles'",
                                                                                                                    numericInput(inputId = 'valorbine6',label = HTML('Inserte la probabilidad del cuantil deseado'),min=0,max=1,step=0.1,value = 0.5,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine7',label = HTML('Seleccione la cantidad de éxito'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine8',label = HTML('Inserte la probabilidad de que ocurra un éxito'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                   conditionalPanel(condition = "input.binega=='Muestra aleatoria'",
                                                                                                                    numericInput(inputId = 'valorbine9',label = HTML('Inserte el tamaño de muestra deseado'),min=0,max=200,step=1,value = 10,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine10',label = HTML('Seleccione la cantidad de éxito'),min=0,max=100,step=1,value = 5,width = '150px'),
                                                                                                                    numericInput(inputId = 'valorbine11',label = HTML('Inserte la probabilidad de que ocurra un éxito'),min=0,max=1,step=0.1,value = 0.5,width = '150px'))),
                                                                                                  conditionalPanel(condition = "input.binega=='Función de densidad'",column(align='center',width=7,br(),verbatimTextOutput("disbinega"),plotOutput("densbinega"))),
                                                                                                  conditionalPanel(condition = "input.binega=='Función de distribución'",column(align='center',width=7,br(),verbatimTextOutput("disbinega1"),plotOutput("densbinega1"))),
                                                                                                  conditionalPanel(condition = "input.binega=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("disbinega2"),plotOutput("densbinega3"))),
                                                                                                  conditionalPanel(condition = "input.binega=='Muestra aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("disbinega3"),plotOutput("densbinega2")))
                                                                                                  )))
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
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )+
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
        geom_segment(aes(x=0,y=1-input$proba1,xend=1,yend=1-input$proba1),size=1,color="blue")+geom_segment(aes(x=1,y=1,xend=1.3,yend=1),size=1,color="blue")+geom_segment(aes(x=1,y=1-input$proba1,xend=1,yend=1),size=1,color="blue",linetype=2)+
        labs( title = "Distribución Bernoulli",
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f1)
  })

  output$bernoulli2<-renderText({

    w<-paste("x = ", qbinom(p=input$valor2,size = 1,prob = input$proba2,lower.tail = TRUE))
    return(w)
  })

  output$dens3<-renderPlot({
    x1<-input$valor2
    p1<-input$proba2

    x2<-qbinom(x1,size=1,prob=p1) #cuantil
    xp <- c(0,1)
    hx <- dbinom(xp,size=1,prob=p1)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                        yend = dbinom(x2, size = 1,prob=p1)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Bernoulli',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
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
           x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
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
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )
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
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f4)
  })

  output$binomial2<-renderText({

    w<-paste("x = ", qbinom(p=input$valorbin2,size = input$ensayobin2,prob = input$probabin2,lower.tail = TRUE))
    return(w)
  })

  output$densbin3<-renderPlot({
    x1<-input$valorbin2
    p1<-input$probabin2
    s1<-input$ensayobin2

    x2<-qbinom(x1,size=s1,prob=p1) #cuantil
    xp <- seq(0,s1,1)
    hx <- dbinom(xp,size=s1,prob=p1)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dbinom(x2, size = s1,prob=p1)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Binomial',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
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
            x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
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
      labs( title = "Densidad Geometrica",
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )
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
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )+
      scale_y_continuous(breaks = seq(0,1,by=0.1),limits = c(0,1))
    return(f6)
  })

  output$geometrica2<-renderText({

    w<-paste("x = ", qgeom(p=input$valorgeo2,prob = input$probageo2,lower.tail = TRUE)+1)
    return(w)
  })


  output$densgeo3<-renderPlot({
    x1<-input$valorgeo2
    p1<-input$probageo2


    x2<-qgeom(x1,prob=p1)+1 #cuantil
    xp <- seq(0,x2+1,1)
    hx <- dgeom(xp,prob=p1)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dgeom(x2,prob=p1)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Geométrica',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
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
            x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
    return(f7)
  })

  output$hiper<-renderText({
    x<-input$valorhip4
    m<-input$valorhip2
    n<-input$valorhip3
    k<-input$valorhip
    resultado6<-paste("f(",x,") = P(X =",x,") = ", dhyper(x,m,n,k))
    return(resultado6)
  })

  output$denship<-renderPlot({
    data8<-data.frame(hiper=dhyper(0:input$valorhip4,input$valorhip2,input$valorhip3,input$valorhip))
    f8<-ggplot(data8,aes(x=0:(length(hiper)-1),y=hiper))+geom_point(colour='blue',size=2)+scale_x_continuous(breaks = 0:(length(data8$hiper)-1))+
      labs( title = "Densidad Hipergeométrica",
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )
    return(f8)
  })

  output$hiper1<-renderText({
    x<-input$valorhip8
    m<-input$valorhip6
    n<-input$valorhip7
    k<-input$valorhip5
    resultado7<-paste("F(",x,") = P(X <=",x,") = ", phyper(x,m,n,k))
    return(resultado7)
  })

  output$denship1<-renderPlot({
    data9<-data.frame(hiper1=phyper(0:input$valorhip8,input$valorhip6,input$valorhip7,input$valorhip5))
    f9<-ggplot(data9,aes(x=0:(length(hiper1)-1),y=hiper1))+geom_step(colour='blue',size=1)+scale_x_continuous(breaks = 0:(length(data9$hiper1)-1))+
      labs( title = "Distribución Hipergeométrica",
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )
    return(f9)
  })

  output$hiper2<-renderText({
    k<-input$valorhip9
    m<-input$valorhip10
    n<-input$valorhip11
    p<-input$valorhip12
    resultado7<-paste("x = ", qhyper(p,m,n,k))
    return(resultado7)
  })

  output$denship3<-renderPlot({
    k<-input$valorhip9
    m<-input$valorhip10
    n<-input$valorhip11
    p<-input$valorhip12

    x2<-qhyper(p,m,n,k) #cuantil
    xp <- seq(0,x2+1,1)
    hx <- dhyper(xp,m,n,k)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dhyper(x2,m,n,k)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Hipergeométrica',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
  })




  muestraber4<-reactive({
    k<-input$valorhip13
    m<-input$valorhip14
    n<-input$valorhip15
    N<-input$valorhip16

    rhyper(N,m,n,k)
  })

  output$hiper3<-renderPrint({
    return(muestraber4())
  })

  output$denship2<-renderPlot({
    data10<-data.frame(x1=muestraber4())
    f10<-ggplot(data10,mapping=aes(x=1:length(x1),y=x1))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data10$x1))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
    return(f10)
  })


  output$dispoison<-renderText({
    x<-input$valorpoi
    p<-input$valorpoi
    resultado10<-paste("f(",x,") = P(X =",x,") = ", dpois(x,p))
    return(resultado10)
  })

  output$denspoi<-renderPlot({
    data11<-data.frame(poi=dpois(0:input$valorpoi2,input$valorpoi))
    f11<-ggplot(data11,aes(x=0:(length(poi)-1),y=poi))+geom_point(colour='blue',size=2)+scale_x_continuous(breaks = 0:(length(data11$poi)-1))+
      labs( title = "Densidad Poisson",
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )
    return(f11)
  })

  output$dispoison1<-renderText({
    p<-input$valorpoi3
    x<-input$valorpoi4
    resultado11<-paste("F(",x,") = P(X <=",x,") = ", ppois(x,p))
    return(resultado11)
  })

  output$denspoi1<-renderPlot({
    data12<-data.frame(poi=ppois(0:input$valorpoi4,input$valorpoi3))
    f12<-ggplot(data12,aes(x=0:(length(poi)-1),y=poi))+geom_step(colour='blue',size=1)+scale_x_continuous(breaks = 0:(length(data12$poi)-1))+
      labs( title = "Distribución Poisson",
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )
    return(f12)
  })

  output$dispoison2<-renderText({
    p<-input$valorpoi5
    x<-input$valorpoi6
    resultado11<-paste("x = ", qpois(x,p))
    return(resultado11)
  })

  output$denspoi3<-renderPlot({
    p1<-input$valorpoi5
    x1<-input$valorpoi6

    x2<-qpois(x1,p1) #cuantil
    xp <- seq(0,x2+1,1)
    hx <- dpois(xp,p1)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dpois(x2,p1)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Poison',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
  })


  muestraber5<-reactive({
    p<-input$valorpoi7
    n<-input$valorpoi8

    rpois(n,p)
  })

  output$dispoison3<-renderPrint({
    return(muestraber5())
  })

  output$denspoi2<-renderPlot({
    data13<-data.frame(x1=muestraber5())
    f13<-ggplot(data13,mapping=aes(x=1:length(x1),y=x1))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data13$x1))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
    return(f13)
  })

  v1<-reactive({
    if(is.null(input$vector2)){
      return()
    } else{
      as.numeric(unlist(strsplit(input$vector2,",")))
    }
  })


  v3<-reactive({
    if(is.null(input$vector)){
      return()
    } else{
      as.numeric(unlist(strsplit(input$vector,",")))
    }
  })



  output$multin<-renderText({
    z1<-v1()
    z3<-v3()
    x<-paste(z1,collapse = ",")
    resultado12<-paste("f(",x,") = P(X =",x,") = ",dmultinom(x=z1,prob=z3))
    return(resultado12)
  })

  v4<-reactive({
    if(is.null(input$vector3)){
      return()
    } else{
      as.numeric(unlist(strsplit(input$vector3,",")))
    }
  })


  muestraber6<-reactive({
    N<-input$vector4
    s<-input$vector5
    p<-v4()

    rmultinom(n=N,size=s,prob = p)
  })

  output$multin1<-renderPrint({
    return(muestraber6())
  })

  # output$densmulti1<-renderPlot({
  #   data14<-data.frame(x2=muestraber6())
  #   f14<-ggplot(data14,mapping=aes(x=1:length(x2),y=x2))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data14$x2))+
  #     labs( title = "Muestra aleatoria",
  #           x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
  #   return(f14)
  # })

  output$disbinega<-renderText({
    x<-input$valorbine
    k<-input$valorbine1
    p<-input$valorbine2
    resultado15<-paste("f(",x,") = P(X =",x,") = ", dnbinom(x= x-k,size = k,prob=p))
    return(resultado15)
  })

  output$densbinega<-renderPlot({
    x<-input$valorbine
    k<-input$valorbine1
    p<-input$valorbine2
    data15<-data.frame(binega=dnbinom(0:(x-k),k,p))
    data16<-data.frame(hola=k,mundo=x)
    f15<-ggplot(data15,aes(x=data16$hola:data16$mundo,y=binega))+geom_point(colour='blue',size=2)+
      labs( title = "Densidad Binomial negativa",
            x = "x", y = "f(x)", caption = "https://synergy.vision/" )
    return(f15)
  })

  output$disbinega1<-renderText({
    x<-input$valorbine3
    k<-input$valorbine4
    p<-input$valorbine5
    resultado16<-paste("F(",x,") = P(X <=",x,") = ", pnbinom(x-k,size = k,prob=p,lower.tail = TRUE))
    return(resultado16)
  })

  output$densbinega1<-renderPlot({
    x<-input$valorbine3
    k<-input$valorbine4
    p<-input$valorbine5
    data16<-data.frame(binega=pnbinom(0:(x-k),k,p,lower.tail = TRUE))

    f16<-ggplot(data16,aes(x=k:x,y=binega))+geom_step(colour='blue',size=1)+
      labs( title = "Distribución Binomial negativa",
            x = "x", y = "F(x)", caption = "https://synergy.vision/" )
    return(f16)
  })

  output$disbinega2<-renderText({
    x<-input$valorbine6
    k<-input$valorbine7
    p<-input$valorbine8
    resultado18<-paste("x = ", qnbinom(x,k,p,lower.tail = TRUE)+k)
    return(resultado18)
  })

  output$densbinega3<-renderPlot({
    x1<-input$valorbine6
    k<-input$valorbine7
    p1<-input$valorbine8

    x2<-qnbinom(x1,k,p1)+k #cuantil
    xp <- seq(0,x2+1,1)
    hx <- dnbinom(xp,k,p1)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_point(colour="blue",size=5)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dnbinom(x2,k,p1)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Binomial Negativa',
            x = "x", y = "f(x)",caption = "https://synergy.vision/" )
    return(f)
  })


  muestraber7<-reactive({
    N<-input$valorbine9
    k<-input$valorbine10
    p<-input$valorbine11

    r<-rnbinom(n=N,size=k,prob = p)
    r+k

  })

  output$disbinega3<-renderPrint({
    return(muestraber7())
  })

  output$densbinega2<-renderPlot({
    data17<-data.frame(x2=muestraber7())
    f17<-ggplot(data17,mapping=aes(x=1:length(x2),y=x2))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data17$x2))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "https://synergy.vision/" )
    return(f17)
  })


  }


shinyApp(ui = ui, server = server)
