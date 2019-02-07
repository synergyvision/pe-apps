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
                                                                                                           numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-40,max=40,step=1,value = 0.5,width = '150px')),
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
                                                                                          conditionalPanel(condition = "input.uni=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("unif2"),plotOutput('cuantiluni'))),
                                                                                          conditionalPanel(condition = "input.uni=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("unif3"),plotOutput("dens3")))
                                                                                          ))),
      conditionalPanel(condition = "input.distribucion=='Exponencial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características",includeHTML("exponencial.html")),
                                                                                tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'exp',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                          conditionalPanel(condition = "input.exp=='Función de Densidad'",
                                                                                                           numericInput(inputId = 'lambda',label = HTML('Seleccione el valor del parámetro &lambda;'),min=0,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                           numericInput(inputId = 'valorexp',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=50,step=0.1,value = 1,width = '150px')),
                                                                                          conditionalPanel(condition = "input.exp=='Función de Distribución'",
                                                                                                           numericInput(inputId = 'lambda1',label = HTML('Seleccione el valor del parámetro &lambda;'),min=0,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                           numericInput(inputId = 'valorexp1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=50,step=0.1,value = 1,width = '150px')),
                                                                                          conditionalPanel(condition = "input.exp=='Cuantiles'",
                                                                                                           numericInput(inputId = 'lambda2',label = HTML('Seleccione el valor del parámetro &lambda;'),min=0,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                           numericInput(inputId = 'valorexp2',label = HTML('Seleccione la probabilidad correspondiente del cuantil'),min=0,max=1,step=0.1,value = 1,width = '150px')),
                                                                                          conditionalPanel(condition = "input.exp=='Muestra Aleatoria'",
                                                                                                           numericInput(inputId = 'lambda3',label = HTML('Seleccione el valor del parámetro &lambda;'),min=0,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                           numericInput(inputId = 'valorexp3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=100,step=1,value = 1,width = '150px'))
                                                                                                                     ),
                                                                                         conditionalPanel(condition = "input.exp=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("expo"),plotOutput("densex"))),
                                                                                         conditionalPanel(condition = "input.exp=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("expo1"),plotOutput("densex1"))),
                                                                                         conditionalPanel(condition = "input.exp=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("expo2"),plotOutput('cuantilexpo'))),
                                                                                         conditionalPanel(condition = "input.exp=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("expo3"),plotOutput("densex2")))
                                                                                         ))),
      conditionalPanel(condition = "input.distribucion=='Gamma'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características",includeHTML("gamma.html")),
                                                                                  tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'gam',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                            conditionalPanel(condition = "input.gam=='Función de Densidad'",
                                                                                                             numericInput(inputId = 'alpha',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'beta',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'valorgamm',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=50,step=0.1,value = 1,width = '150px')),
                                                                                            conditionalPanel(condition = "input.gam=='Función de Distribución'",
                                                                                                             numericInput(inputId = 'alpha1',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'beta1',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'valorgamm1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=50,step=0.1,value = 1,width = '150px')),
                                                                                            conditionalPanel(condition = "input.gam=='Cuantiles'",
                                                                                                             numericInput(inputId = 'alpha2',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'beta2',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'valorgamm2',label = HTML('Seleccione el valor de la probabilidad del cuantil'),min=0,max=1,step=0.1,value = 1,width = '150px')),
                                                                                            conditionalPanel(condition = "input.gam=='Muestra Aleatoria'",
                                                                                                             numericInput(inputId = 'alpha3',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'beta3',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                             numericInput(inputId = 'valorgamm3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=100,step=1,value = 10,width = '150px'))
                                                                                                                       ),
                                                                                           conditionalPanel(condition = "input.gam=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("gamma"),plotOutput("densgam"))),
                                                                                           conditionalPanel(condition = "input.gam=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("gamma1"),plotOutput("densgam1"))),
                                                                                           conditionalPanel(condition = "input.gam=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("gamma2"),plotOutput('cuantilgamma'))),
                                                                                           conditionalPanel(condition = "input.gam=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("gamma3"),plotOutput("densgam2")))
                                                                                           ))),
      conditionalPanel(condition = "input.distribucion=='Beta'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características",includeHTML("beta.html")),
                                                                                       tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'bet',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                                conditionalPanel(condition = "input.bet=='Función de Densidad'",
                                                                                                                 numericInput(inputId = 'alphab',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'betab',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'valorbeta',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                conditionalPanel(condition = "input.bet=='Función de Distribución'",
                                                                                                                 numericInput(inputId = 'alphab1',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'betab1',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'valorbeta1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                conditionalPanel(condition = "input.bet=='Cuantiles'",
                                                                                                                 numericInput(inputId = 'alphab2',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'betab2',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'valorbeta2',label = HTML('Seleccione el valor de la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                                conditionalPanel(condition = "input.bet=='Muestra Aleatoria'",
                                                                                                                 numericInput(inputId = 'alphab3',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'betab3',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'valorbeta3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=0,max=100,step=1,value = 10,width = '150px'))
                                                                                                                            ),
                                                                                                conditionalPanel(condition = "input.bet=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("fbeta"),plotOutput("densfbeta"))),
                                                                                                conditionalPanel(condition = "input.bet=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("fbeta1"),plotOutput("densfbeta1"))),
                                                                                                conditionalPanel(condition = "input.bet=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("fbeta2"),plotOutput('cuantilbeta'))),
                                                                                                conditionalPanel(condition = "input.bet=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("fbeta3"),plotOutput("densfbeta2")))
                                                                                                ))),
      conditionalPanel(condition = "input.distribucion=='Chi-cuadrado'",tabsetPanel(type = "pills", id="pri5",tabPanel("Características",includeHTML('Chi-cuadrado.html')),
                                                                                   tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'chi',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                   conditionalPanel(condition = "input.chi=='Función de Densidad'",
                                                                                                    numericInput(inputId = 'df',label = HTML('Seleccione los grados de libertad'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                    numericInput(inputId = 'valorchi',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=100,step=0.1,value = 1,width = '150px')),
                                                                                   conditionalPanel(condition = "input.chi=='Función de Distribución'",
                                                                                                    numericInput(inputId = 'df1',label = HTML('Seleccione los grados de libertad'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                    numericInput(inputId = 'valorchi1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=100,step=0.1,value = 1,width = '150px')),
                                                                                   conditionalPanel(condition = "input.chi=='Cuantiles'",
                                                                                                    numericInput(inputId = 'df2',label = HTML('Seleccione los grados de libertad'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                    numericInput(inputId = 'valorchi2',label = HTML('Seleccione la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                   conditionalPanel(condition = "input.chi=='Muestra Aleatoria'",
                                                                                                    numericInput(inputId = 'df3',label = HTML('Seleccione los grados de libertad'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                    numericInput(inputId = 'valorchi3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=200,step=1,value = 10,width = '150px'))
                                                                                   
                                                                                                                        ),
                                                                                   conditionalPanel(condition = "input.chi=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("fchi"),plotOutput("denschi"))),
                                                                                   conditionalPanel(condition = "input.chi=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("fchi1"),plotOutput("denschi1"))),
                                                                                   conditionalPanel(condition = "input.chi=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("fchi2"),plotOutput('cuantilchi'))),
                                                                                   conditionalPanel(condition = "input.chi=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("fchi3"),plotOutput("denschi2")))
                                                                                            ))),
      conditionalPanel(condition = "input.distribucion=='Fisher-Snedecor'",tabsetPanel(type = "pills", id="pri6",tabPanel("Características",includeHTML('Fisher-snedcor.html')),
                                                                               tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'fish',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                               conditionalPanel(condition = "input.fish=='Función de Densidad'",
                                                                                                numericInput(inputId = 'dfisher1',label = HTML('Seleccione los grados de libertad <i>n<sub>1</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'dfisher2',label = HTML('Seleccione los grados de libertad <i>n<sub>2</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorfish',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=100,step=0.1,value = 1,width = '150px')),
                                                                               conditionalPanel(condition = "input.fish=='Función de Distribución'",
                                                                                                numericInput(inputId = 'dfisher3',label = HTML('Seleccione los grados de libertad <i>n<sub>1</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'dfisher4',label = HTML('Seleccione los grados de libertad <i>n<sub>2</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorfish1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=100,step=0.1,value = 1,width = '150px')),
                                                                               conditionalPanel(condition = "input.fish=='Cuantiles'",
                                                                                                numericInput(inputId = 'dfisher5',label = HTML('Seleccione los grados de libertad <i>n<sub>1</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'dfisher6',label = HTML('Seleccione los grados de libertad <i>n<sub>2</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorfish2',label = HTML('Seleccione la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                               conditionalPanel(condition = "input.fish=='Muestra Aleatoria'",
                                                                                                numericInput(inputId = 'dfisher7',label = HTML('Seleccione los grados de libertad <i>n<sub>1</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'dfisher8',label = HTML('Seleccione los grados de libertad <i>n<sub>2</sub></i>'),min=0,max=100,step=1,value = 1,width = '150px'),
                                                                                                numericInput(inputId = 'valorfish3',label = HTML('Seleccione el tamaño de la muestra asociada'),min=0,max=100,step=1,value = 10,width = '150px'))
                                                                                                                    ),
                                                                               conditionalPanel(condition = "input.fish=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("fisher"),plotOutput("densfi"))),
                                                                               conditionalPanel(condition = "input.fish=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("fisher1"),plotOutput("densfi1"))),
                                                                               conditionalPanel(condition = "input.fish=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("fisher2"),plotOutput('cuantilfish'))),
                                                                               conditionalPanel(condition = "input.fish=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("fisher3"),plotOutput("densfi2")))
                                                                                        ))),
      conditionalPanel(condition = "input.distribucion=='t-Student'",tabsetPanel(type = "pills", id="pri7",tabPanel("Características",includeHTML('t-Student.html')),
                                                                                         tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 't',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                         conditionalPanel(condition = "input.t=='Función de Densidad'",
                                                                                                          numericInput(inputId = 'dft',label = HTML('Seleccione los grados de libertad'),min=1,max=100,step=1,value = 3,width = '150px'),
                                                                                                          numericInput(inputId = 'valort',label = HTML('Seleccione el valor de la función de densidad'),min=-50,max=50,step=0.1,value = 1,width = '150px')),
                                                                                         conditionalPanel(condition = "input.t=='Función de Distribución'",
                                                                                                          numericInput(inputId = 'dft1',label = HTML('Seleccione los grados de libertad'),min=1,max=100,step=1,value = 3,width = '150px'),
                                                                                                          numericInput(inputId = 'valort1',label = HTML('Seleccione el valor de la función de distribución'),min=-50,max=50,step=0.1,value = 1,width = '150px')),
                                                                                         conditionalPanel(condition = "input.t=='Cuantiles'",
                                                                                                          numericInput(inputId = 'dft2',label = HTML('Seleccione los grados de libertad'),min=1,max=100,step=1,value = 3,width = '150px'),
                                                                                                          numericInput(inputId = 'valort2',label = HTML('Seleccione el valor de la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                         conditionalPanel(condition = "input.t=='Muestra Aleatoria'",
                                                                                                          numericInput(inputId = 'dft3',label = HTML('Seleccione los grados de libertad'),min=1,max=100,step=1,value = 3,width = '150px'),
                                                                                                          numericInput(inputId = 'valort3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=100,step=1,value = 10,width = '150px'))
                                                                                                          ),
                                                                                         conditionalPanel(condition = "input.t=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("student"),plotOutput("denst"))),
                                                                                         conditionalPanel(condition = "input.t=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("student1"),plotOutput("denst1"))),
                                                                                         conditionalPanel(condition = "input.t=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("student2"),plotOutput('cuantilt'))),
                                                                                         conditionalPanel(condition = "input.t=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("student3"),plotOutput("denst2")))
                                                                                         
                                                                                                  ))),
      conditionalPanel(condition = "input.distribucion=='Weibull'",tabsetPanel(type = "pills", id="pri8",tabPanel("Características",includeHTML('Weibull.html')),
                                                                                         tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'wei',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                                         conditionalPanel(condition = "input.wei=='Función de Densidad'",
                                                                                                          numericInput(inputId = 'alphaw',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                          numericInput(inputId = 'betaw',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                          numericInput(inputId = 'valorwei',label = HTML('Seleccione el valor de la función de densidad'),min=0,max=50,step=0.1,value = 0.5,width = '150px')),
                                                                                         conditionalPanel(condition = "input.wei=='Función de Distribución'",
                                                                                                          numericInput(inputId = 'alphaw1',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                          numericInput(inputId = 'betaw1',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 5,width = '150px'),
                                                                                                          numericInput(inputId = 'valorwei1',label = HTML('Seleccione el valor de la función de distribución'),min=0,max=50,step=0.1,value = 3,width = '150px')),
                                                                                         conditionalPanel(condition = "input.wei=='Cuantiles'",
                                                                                                          numericInput(inputId = 'alphaw2',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                          numericInput(inputId = 'betaw2',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 5,width = '150px'),
                                                                                                          numericInput(inputId = 'valorwei2',label = HTML('Seleccione el valor de la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                                         conditionalPanel(condition = "input.wei=='Muestra Aleatoria'",
                                                                                                          numericInput(inputId = 'alphaw3',label = HTML('Seleccione el valor del parámetro &alpha;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                                          numericInput(inputId = 'betaw3',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 5,width = '150px'),
                                                                                                          numericInput(inputId = 'valorwei3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=100,step=1,value = 10,width = '150px'))
                                                                                                                              ),
                                                                                         conditionalPanel(condition = "input.wei=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("weib"),plotOutput("denswei"))),
                                                                                         conditionalPanel(condition = "input.wei=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("weib1"),plotOutput("denswei1"))),
                                                                                         conditionalPanel(condition = "input.wei=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("weib2"),plotOutput('cuantilwei'))),
                                                                                         conditionalPanel(condition = "input.wei=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("weib3"),plotOutput("denswei2")))
                                                                                                  ))),
      conditionalPanel(condition = "input.distribucion=='Cauchy'",tabsetPanel(type = "pills", id="pri9",tabPanel("Características",includeHTML('Cauchy.html')),
                                                                              tabPanel('Cálculos',br(),br(),column(width=5,selectInput(inputId = 'cau',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Cuantiles','Muestra Aleatoria'),selected = NULL),
                                                                              conditionalPanel(condition = "input.cau=='Función de Densidad'",
                                                                                               numericInput(inputId = 'alphacau',label = HTML('Seleccione el valor del parámetro &alpha;'),min=-20,max=20,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'betacau',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'valorcau',label = HTML('Seleccione el valor de la función de densidad'),min=-20,max=20,step=0.1,value = 0,width = '150px')),
                                                                              conditionalPanel(condition = "input.cau=='Función de Distribución'",
                                                                                               numericInput(inputId = 'alphacau1',label = HTML('Seleccione el valor del parámetro &alpha;'),min=-20,max=20,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'betacau1',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'valorcau1',label = HTML('Seleccione el valor de la función de distribución'),min=-20,max=20,step=0.1,value = 10,width = '150px')),
                                                                              conditionalPanel(condition = "input.cau=='Cuantiles'",
                                                                                               numericInput(inputId = 'alphacau2',label = HTML('Seleccione el valor del parámetro &alpha;'),min=-20,max=20,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'betacau2',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'valorcau2',label = HTML('Seleccione el valor de la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                                                                              conditionalPanel(condition = "input.cau=='Muestra Aleatoria'",
                                                                                               numericInput(inputId = 'alphacau3',label = HTML('Seleccione el valor del parámetro &alpha;'),min=-20,max=20,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'betacau3',label = HTML('Seleccione el valor parámetro &beta;'),min=0.1,max=50,step=0.1,value = 1,width = '150px'),
                                                                                               numericInput(inputId = 'valorcau3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=100,step=1,value = 10,width = '150px'))
                                                                              ),
                                                                              conditionalPanel(condition = "input.cau=='Función de Densidad'",column(align='center',width=7,br(),verbatimTextOutput("cauy"),plotOutput("denscau"))),
                                                                              conditionalPanel(condition = "input.cau=='Función de Distribución'",column(align='center',width=7,br(),verbatimTextOutput("cauy1"),plotOutput("denscau1"))),
                                                                              conditionalPanel(condition = "input.cau=='Cuantiles'",column(align='center',width=7,br(),verbatimTextOutput("cauy2"),plotOutput('cuantilcau'))),
                                                                              conditionalPanel(condition = "input.cau=='Muestra Aleatoria'",column(align='center',width=7,br(),verbatimTextOutput("cauy3"),plotOutput("denscau2")))
                                                                                       )))
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
    
    x <- seq(-1+l,1+u,0.01)
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
  
  muestrau1<-reactive({
    x<-input$valor3
    l<-input$rangouni3[1]
    u<-input$rangouni3[2]
    
    return(round(runif(x,l,u),2))
  })
  
  output$unif3<-renderPrint({
    return(muestrau1())
  })
  
  output$dens3<-renderPlot({
    data2<-data.frame(x=muestrau1())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$expo<-renderText({
    x<-input$valorexp
    l<-input$lambda
    resultado<-paste("f(",x,") = ", dexp(x,l))
    return(resultado)
  })
  
  output$densex<-renderPlot({
    x1<-input$valorexp
    
    l<-input$lambda
    x <- seq(0,50,0.01)
    hx <- dexp(x,l)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dexp(x1,l)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad exponencial',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$expo1<-renderText({
    x<-input$valorexp1
    l<-input$lambda1
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pexp(x,rate = l))
    return(resultado1)
  })
  
  #revisar
  output$densex1<-renderPlot({
    x<-input$valorexp1
    l<-input$lambda1
    data1<-data.frame(exp=pexp(seq(0,x,0.1),rate=l,lower.tail = TRUE))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=exp))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Exponencial",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  output$expo2<-renderText({
    x<-input$valorexp2
    l<-input$lambda2
    
    resultado2<-paste("x = ", qexp(x,l))
    return(resultado2)
  })
  
  muestraexp<-reactive({
    x<-input$valorexp3
    l<-input$lambda3
    
    return(round(rexp(x,l),2))
  })
  
  output$expo3<-renderPrint({
    return(muestraexp())
  })
  
  output$densex2<-renderPlot({
    data2<-data.frame(x=muestraexp())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$gamma<-renderText({
    x<-input$valorgamm
    alpha<-input$alpha
    beta<-input$beta
    resultado<-paste("f(",x,") = ", dgamma(x,shape = alpha,scale = beta))
    return(resultado)
  })
  
  output$densgam<-renderPlot({
    x1<-input$valorgamm
    
    alpha<-input$alpha
    beta<-input$beta
    x <- seq(0,50,0.01)
    hx <- dgamma(x,shape = alpha,scale = beta)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dgamma(x1,shape = alpha,scale = beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad gamma',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$gamma1<-renderText({
    x<-input$valorgamm1
    
    alpha<-input$alpha1
    beta<-input$beta1
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pgamma(x,shape = alpha,scale = beta))
    return(resultado1)
  })
  

  output$densgam1<-renderPlot({
    x<-input$valorgamm1
    
    alpha<-input$alpha1
    beta<-input$beta1
    
    data1<-data.frame(gam=pgamma(seq(0,x,0.1),shape = alpha,scale = beta,lower.tail = TRUE))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=gam))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Gamma",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$gamma2<-renderText({
    x<-input$valorgamm2
    alpha<-input$alpha2
    beta<-input$beta2
    
    resultado2<-paste("x = ", qgamma(x,shape = alpha,scale = beta))
    return(resultado2)
  })
  
  muestragamm<-reactive({
    x<-input$valorgamm3
    alpha<-input$alpha3
    beta<-input$beta3
    return(round(rgamma(x,shape = alpha,scale = beta),2))
  })
  
  output$gamma3<-renderPrint({
    return(muestragamm())
  })
  
  output$densgam2<-renderPlot({
    data2<-data.frame(x=muestragamm())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$fbeta<-renderText({
    x<-input$valorbeta
    alpha<-input$alphab
    beta<-input$betab
    resultado<-paste("f(",x,") = ", dbeta(x,shape1 = alpha,shape2 = beta))
    return(resultado)
  })
  
  output$densfbeta<-renderPlot({
    x1<-input$valorbeta
    
    alpha<-input$alphab
    beta<-input$betab
    x <- seq(-0.3,1.3,0.01)
    hx <- dbeta(x,shape1 = alpha,shape2 = beta)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dbeta(x1,shape1 = alpha,shape2 = beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad beta',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$fbeta1<-renderText({
    x<-input$valorbeta1
    alpha<-input$alphab1
    beta<-input$betab1
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pbeta(x,shape1 = alpha,shape2 = beta))
    return(resultado1)
  })
  
  
  output$densfbeta1<-renderPlot({
    x<-input$valorbeta1
    alpha<-input$alphab1
    beta<-input$betab1
    
    data1<-data.frame(beta=pbeta(seq(0,x,0.1),shape1 = alpha,shape2 = beta,lower.tail = TRUE))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=beta))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Beta",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$fbeta2<-renderText({
    x<-input$valorbeta2
    alpha<-input$alphab2
    beta<-input$betab2
    
    resultado2<-paste("x = ", qbeta(x,shape1 = alpha,shape2 = beta))
    return(resultado2)
  })
  
  muestrabeta<-reactive({
    x<-input$valorbeta3
    alpha<-input$alphab3
    beta<-input$betab3
    return(round(rbeta(x,shape1 = alpha,shape2 = beta),2))
  })
  
  output$fbeta3<-renderPrint({
    return(muestrabeta())
  })
  
  output$densfbeta2<-renderPlot({
    data2<-data.frame(x=muestrabeta())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$fchi<-renderText({
    x<-input$valorchi
    gl<-input$df
    
    resultado<-paste("f(",x,") = ", dchisq(x,df=gl))
    return(resultado)
  })
  
  output$denschi<-renderPlot({
    x1<-input$valorchi
    
    gl<-input$df
    
    x <- seq(0,100,0.01)
    hx <- dchisq(x,df=gl)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dchisq(x1,df=gl)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Chi-Cuadrado',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  
  output$fchi1<-renderText({
    x<-input$valorchi1
    g<-input$df1
    
   
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pchisq(x,df=g))
    return(resultado1)
  })
  
  
  output$denschi1<-renderPlot({
    x<-input$valorchi1
    g<-input$df1
    
    data1<-data.frame(chi=pchisq(seq(0,x,0.1),df=g))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=chi))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Chi-cuadrado",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$fchi2<-renderText({
    x<-input$valorchi2
    g<-input$df2
    
    resultado2<-paste("x = ", qchisq(x,df=g))
    return(resultado2)
  })
  
  
  muestrachi<-reactive({
    x<-input$valorchi3
    g<-input$df3
    return(round(rchisq(x,df=g),2))
  })
  
  output$fchi3<-renderPrint({
    return(muestrachi())
  })
  
  output$denschi2<-renderPlot({
    data2<-data.frame(x=muestrachi())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$fisher<-renderText({
    x<-input$valorfish
    gl1<-input$dfisher1
    gl2<-input$dfisher2
    
    resultado<-paste("f(",x,") = ", df(x,df1=gl1,df2=gl2))
    return(resultado)
  })
  
  output$densfi<-renderPlot({
    x1<-input$valorfish
    
    gl1<-input$dfisher1
    gl2<-input$dfisher2
    
    x <- seq(0,30,0.01)
    hx <- df(x,df1=gl1,df2 = gl2)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = df(x1,df1=gl1,df2=gl2)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Fisher',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$fisher1<-renderText({
    x<-input$valorfish1
    g1<-input$dfisher3
    g2<-input$dfisher4
    
    
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pf(x,df1=g1,df2 = g2))
    return(resultado1)
  })
  
  
  output$densfi1<-renderPlot({
    x<-input$valorfish1
    g1<-input$dfisher3
    g2<-input$dfisher4
    
    data1<-data.frame(fi=pf(seq(0,x,0.1),df1=g1,df2 = g2))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=fi))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Fisher",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$fisher2<-renderText({
    x<-input$valorfish2
    g1<-input$dfisher5
    g2<-input$dfisher6
    
    resultado2<-paste("x = ", qf(x,df1=g1,df2 = g2))
    return(resultado2)
  })
  
  muestrafish<-reactive({
    x<-input$valorfish3
    g1<-input$dfisher7
    g2<-input$dfisher8
    return(round(rf(x,df1=g1,df2 = g2),2))
  })
  
  output$fisher3<-renderPrint({
    return(muestrafish())
  })
  
  output$densfi2<-renderPlot({
    data2<-data.frame(x=muestrafish())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$student<-renderText({
    x<-input$valort
    gl<-input$dft
    
    resultado<-paste("f(",x,") = ", dt(x,df=gl))
    return(resultado)
  })
  
  output$denst<-renderPlot({
    x1<-input$valort

    gl<-input$dft

    x <- seq(-6,6,0.01)
    hx <- dt(x,df=gl)

    dat<-data.frame(x,hx)

    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dt(x1,df=gl)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad t-Student',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$student1<-renderText({
    x<-input$valort1
    g<-input$dft1
    
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pt(x,df=g))
    return(resultado1)
  })
  
  
  output$denst1<-renderPlot({
    x<-input$valort1
    g<-input$dft1
    
    data1<-data.frame(t=pt(seq(-6,x,0.1),df=g))
    
    f1<-ggplot(data1,aes(x=seq(-6,x,0.1),y=t))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución T-student",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$student2<-renderText({
    x<-input$valort2
    g<-input$dft2
    
    resultado2<-paste("x = ", qt(x,df=g))
    return(resultado2)
  })
  
  muestrats<-reactive({
    x<-input$valort3
    g<-input$dft3
    return(round(rt(x,df=g),2))
  })

  output$student3<-renderPrint({
    return(muestrats())
  })
  
  output$denst2<-renderPlot({
    data2<-data.frame(x=muestrats())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$weib<-renderText({
    x<-input$valorwei
    alpha<-input$alphaw
    beta<-input$betaw
    
    resultado<-paste("f(",x,") = ", dweibull(x,shape=beta,scale=alpha))
    return(resultado)
  })
  
  output$denswei<-renderPlot({
    x1<-input$valorwei
    
    alpha<-input$alphaw
    beta<-input$betaw
    
    x <- seq(0,10,0.01)
    hx <- dweibull(x,shape=beta,scale=alpha)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dweibull(x1,shape=beta,scale=alpha)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Weibull',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$weib1<-renderText({
    x<-input$valorwei1
    alpha<-input$alphaw1
    beta<-input$betaw1
    
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pweibull(x,shape = beta,scale = alpha))
    return(resultado1)
  })
  
  
  output$denswei1<-renderPlot({
    x<-input$valorwei1
    alpha<-input$alphaw1
    beta<-input$betaw1
    
    data1<-data.frame(wei=pweibull(seq(0,x,0.1),shape = beta,scale = alpha))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=wei))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Weibull",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  output$weib2<-renderText({
    x<-input$valorwei2
    alpha<-input$alphaw2
    beta<-input$betaw2
    
    resultado2<-paste("x = ", qweibull(x,shape = beta,scale = alpha))
    return(resultado2)
  })
  
  muestrawei<-reactive({
    x<-input$valorwei3
    alpha<-input$alphaw3
    beta<-input$betaw3
    
    return(round(rweibull(x,shape = beta,scale = alpha),2))
  })
  
  output$weib3<-renderPrint({
    return(muestrawei())
  })
  
  output$denswei2<-renderPlot({
    data2<-data.frame(x=muestrawei())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$cauy<-renderText({
    x<-input$valorcau
    alpha<-input$alphacau
    beta<-input$betacau
    
    resultado<-paste("f(",x,") = ", dcauchy(x,location = alpha,scale=beta))
    return(resultado)
  })
  
  output$denscau<-renderPlot({
    x1<-input$valorcau
    
    alpha<-input$alphacau
    beta<-input$betacau
    
    x <- seq(-20,20,0.01)
    hx <- dcauchy(x,location = alpha,scale=beta)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dcauchy(x1,location = alpha,scale=beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Cauchy',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cauy1<-renderText({
    x<-input$valorcau1
    alpha<-input$alphacau1
    beta<-input$betacau1
    
    resultado1<-paste("F(",x,") = P(X <=",x,") = ", pcauchy(x,location = alpha,scale = beta))
    return(resultado1)
  })
  
  
  output$denscau1<-renderPlot({
    x<-input$valorcau1
    alpha<-input$alphacau1
    beta<-input$betacau1
    
    data1<-data.frame(cau=pcauchy(seq(0,x,0.1),location = alpha,scale = beta))
    
    f1<-ggplot(data1,aes(x=seq(0,x,0.1),y=cau))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Cauchy",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  output$cauy2<-renderText({
    x<-input$valorcau2
    alpha<-input$alphacau2
    beta<-input$betacau2
    
    
    resultado2<-paste("x = ", qcauchy(x,location = alpha,scale = beta))
    return(resultado2)
  })
  
  
  muestracau<-reactive({
    x<-input$valorcau3
    alpha<-input$alphacau3
    beta<-input$betacau3
    
    return(round(rcauchy(x,location = alpha,scale = beta),2))
  })
  
  output$cauy3<-renderPrint({
    return(muestracau())
  })
  
  output$denscau2<-renderPlot({
    data2<-data.frame(x=muestracau())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  #Grafica de los cuantiles
  
  output$cuantiluni<-renderPlot({
    x1<-input$valor2
    l<-input$rangouni2[1]
    u<-input$rangouni2[2]
    
    x2<-qunif(x1,l,u)
    
    x <- seq(-1+l,1+u,0.01)
    hx <- dunif(x,l,u)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x= ifelse(x >= l & x <= u,x,x)), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dunif(x2,l,u)),
                   colour = "black",linetype=2)+
      labs( title = 'Cuantil',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilexpo<-renderPlot({
    x1<-input$valorexp2
    
    l<-input$lambda2
    x <- seq(0,50,0.01)
    hx <- dexp(x,l)
    
    x2<-qexp(x1,l)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dexp(x2,l)),
                   colour = "black",linetype=2)+
      labs( title = 'Cuantil',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilgamma<-renderPlot({
    x1<-input$valorgamm2
    
    alpha<-input$alpha2
    beta<-input$beta2
    x <- seq(0,50,0.01)
    hx <- dgamma(x,shape = alpha,scale = beta)
    
    x2<-qgamma(x1,shape = alpha,scale = beta)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dgamma(x2,shape = alpha,scale = beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Cuantil',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilbeta<-renderPlot({
    x1<-input$valorbeta2
    
    alpha<-input$alphab2
    beta<-input$betab2
    x <- seq(-0.3,1.3,0.01)
    hx <- dbeta(x,shape1 = alpha,shape2 = beta)
    
    x2<-qbeta(x1,shape1 = alpha,shape2 = beta)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dbeta(x2,shape1 = alpha,shape2 = beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Cuantil',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilchi<-renderPlot({
    x1<-input$valorchi2
    
    gl<-input$df2
    
    x <- seq(0,100,0.01)
    hx <- dchisq(x,df=gl)
    
    x2<-qchisq(x1,gl)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dchisq(x2,df=gl)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Chi-Cuadrado',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilfish<-renderPlot({
    x1<-input$valorfish2
    
    gl1<-input$dfisher5
    gl2<-input$dfisher6
    
    x <- seq(0,30,0.01)
    hx <- df(x,df1=gl1,df2 = gl2)
    
    dat<-data.frame(x,hx)
    
    x2<-qf(x1,gl1,gl2)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = df(x2,df1=gl1,df2=gl2)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Fisher',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilt<-renderPlot({
    x1<-input$valort2
    
    gl<-input$dft2
    
    x <- seq(-6,6,0.01)
    hx <- dt(x,df=gl)
    
    x2<-qt(x1,gl)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dt(x2,df=gl)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad t-Student',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilwei<-renderPlot({
    x1<-input$valorwei2
    
    alpha<-input$alphaw2
    beta<-input$betaw2
    
    x <- seq(0,10,0.01)
    hx <- dweibull(x,shape=beta,scale=alpha)
    
    dat<-data.frame(x,hx)
    
    x2<-qweibull(x1,shape = beta,scale = alpha)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dweibull(x2,shape=beta,scale=alpha)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Weibull',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  output$cuantilcau<-renderPlot({
    x1<-input$valorcau2
    
    alpha<-input$alphacau2
    beta<-input$betacau2
    
    x <- seq(-20,20,0.01)
    hx <- dcauchy(x,location = alpha,scale=beta)
    
    dat<-data.frame(x,hx)
    
    x2<-qcauchy(x1,location = alpha,scale = beta)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dcauchy(x2,location = alpha,scale=beta)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Cauchy',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )
    return(f)
  })
  
  
  }


shinyApp(ui = ui, server = server)
