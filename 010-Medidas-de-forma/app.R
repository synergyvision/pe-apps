ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("ggplot2", "3.0.0")
ensure_version("readxl", "1.1.0")

library(shiny)
library(ggplot2)
library(readxl)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Medidas de forma"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      column(width=4,div(style="height:400px; overflow-y: scroll",tableOutput("table"))),
      column(width=8,verbatimTextOutput(outputId = "texto"))
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  Sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  Horas<-c(rep(2,46),rep(3,15),rep(4,12),rep(6,52),rep(7,8))
  
  Otros<-c(rep(10,4),rep(22,5),rep(35,2),rep(46,10),rep(57,9),rep(68,6),rep(74,6))
  
  
  dat<-reactive({
    
    infile <- input$n
    if(is.null(infile)){
      return()
      }
    
    else if(infile=='Ejemplos del libro'){
      
      infile1<-input$m
      
      if(infile1=='Sueldos'){
       data.frame(Sueldos)
      }
      
      else if(infile1=='Tiempo de uso de equipos'){
        data.frame(Horas)
      }
      
      else if(infile1=='Otros')
        data.frame(Otros)
    }
    
    else if(infile=='Cargados'){
      infile2<-input$datoscargados
      if(is.null(infile2)){
        return()
      }
      
      else{
        as.data.frame(read_excel(infile2$datapath))
      }
    }
    
    else if(infile=='Generados aleatoriamente'){
      data.frame(Datos=sample(80:100,input$CantidadDatos,replace = TRUE))
    }
    
    })
  
  output$table<-renderTable({
    return(dat())
  },digits = 1)
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
