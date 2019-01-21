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

  titlePanel("Distribuciones Continuas"),
  sidebarLayout(

    sidebarPanel(

      selectInput(inputId = 'distribucion',label = HTML('Seleccione la distribución deseada'),choices = c('Uniforme','Exponencial','Gamma','Beta','Chi-cuadrado','Fisher-Snedecor','t-Student','Weibull','Cauchy'),selected = NULL)
      ),

    mainPanel(

      withMathJax(),
      conditionalPanel(condition = "input.distribucion=='Uniforme'",tabsetPanel(type = "pills", id="pri",tabPanel('Características'),
                                                                                 tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Exponencial'",tabsetPanel(type = "pills", id="pri2",tabPanel("Características"),
                                                                                tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Gamma'",tabsetPanel(type = "pills", id="pri3",tabPanel("Características"),
                                                                                  tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Beta'",tabsetPanel(type = "pills", id="pri4",tabPanel("Características"),
                                                                                       tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Chi-cuadrado'",tabsetPanel(type = "pills", id="pri5",tabPanel("Características"),
                                                                                   tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Fisher-Snedecor'",tabsetPanel(type = "pills", id="pri6",tabPanel("Características"),
                                                                               tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='t-Student'",tabsetPanel(type = "pills", id="pri7",tabPanel("Características"),
                                                                                         tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Weibull'",tabsetPanel(type = "pills", id="pri8",tabPanel("Características"),
                                                                                         tabPanel('Cálculos',br(),br()))),
      conditionalPanel(condition = "input.distribucion=='Cauchy'",tabsetPanel(type = "pills", id="pri9",tabPanel("Características"),
                                                                              tabPanel('Cálculos',br(),br())))
    )
  )
)



server <- function(input, output,session) {

  }


shinyApp(ui = ui, server = server)
