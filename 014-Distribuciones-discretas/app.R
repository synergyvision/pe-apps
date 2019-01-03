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
      conditionalPanel(condition = "input.distribucion=='Bernoulli'",tabBox(width = 12,title = "",id="tab1",tabPanel('Características',includeMarkdown("bernoulli.Rmd")),
                                                                            tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Binomial'",tabBox(width=12,title="",id="tab2",tabPanel("Características",includeMarkdown("binomial.Rmd")),
                       tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Geométrica'",tabBox(width=12,title="",id="tab3",tabPanel("Características",includeMarkdown("geometrica.Rmd")),
                                                                             tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Hipergeométrica'",tabBox(width=12,title="",id="tab4",tabPanel("Características",includeMarkdown("hipergeometrica.Rmd")),
                                                                                  tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Multinomial'",tabBox(width=12,title="",id="tab5",tabPanel("Características",includeMarkdown("multinomial.Rmd")),
                                                                              tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Poisson'",tabBox(width=12,title="",id="tab6",tabPanel("Características",includeMarkdown("poisson.Rmd")),
                                                                          tabPanel('Cálculos',br(),br(),"prueba"))),
      conditionalPanel(condition = "input.distribucion=='Binomial negativa'",tabBox(width=12,title="",id="tab7",tabPanel("Características",includeMarkdown("binonegativa.Rmd")),
      tabPanel('Cálculos',br(),br(),"prueba")))
    )
  )
)



server <- function(input, output,session) {
  
  }


shinyApp(ui = ui, server = server)
