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
<<<<<<< HEAD
      
=======
>>>>>>> 81ebaa7477c203a0ee0f8ac9064d3ac1b70cd9f5
      withMathJax(),
      conditionalPanel(condition = "input.distribucion=='Bernoulli'",h1(HTML("$$\\alpha$$"))),
      conditionalPanel(condition = "input.distribucion=='Binomial'",fluidRow(h1(HTML("$$\\alpha^2$$"))),
                       h3('Funcion de densidad'),fluidRow(h1(HTML("$$f(x)=x$$")))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",h1(HTML("$$\\alpha+\\epsilon$$"))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",h1(HTML("$$\\pi$$"))),
      conditionalPanel(condition = "input.distribucion=='Multinomial'",h1(HTML("$$\\alpha^2+\\epsilon$$"))),
      conditionalPanel(condition = "input.distribucion=='Poisson'",h1(HTML("$$\\alpha+\\beta$$"))),
      conditionalPanel(condition = "input.distribucion=='Binomial negativa'",h1(HTML("$$\\alpha^2+\\beta$$")))
    )
  )
)



server <- function(input, output,session) {
  
  }


shinyApp(ui = ui, server = server)
