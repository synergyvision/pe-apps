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
       conditionalPanel(condition = "input.distribucion=='Bernoulli'",includeMarkdown("bernoulli.Rmd")),
      conditionalPanel(condition = "input.distribucion=='Binomial'",fluidRow(h1(HTML("$$\\alpha^2$$"))),
                       h3('Funcion de densidad'),fluidRow(h1(HTML("$$f(x)=x$$")))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",h3('Función de Densidad'),fluidRow(h1(HTML("$$f(x) =(1-p)^{x-1} p$$"))),
                       h3('Función de Distribución'),fluidRow(h1(HTML("$$F(x) = 1- (1-p)^{x+1}$$"))),
                       h3('Media'),fluidRow(h1(HTML("$$\\mathbb{E[x]}=\\displaystyle \\frac{1}{p}$$")))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",includeMarkdown("hipergeometrica.Rmd")),
      conditionalPanel(condition = "input.distribucion=='Multinomial'",includeMarkdown("multinomial.Rmd")),
      conditionalPanel(condition = "input.distribucion=='Poisson'",includeMarkdown("poisson.Rmd")),
      conditionalPanel(condition = "input.distribucion=='Binomial negativa'",includeMarkdown("binonegativa.Rmd"))
    )
  )
)



server <- function(input, output,session) {
  
  }


shinyApp(ui = ui, server = server)
