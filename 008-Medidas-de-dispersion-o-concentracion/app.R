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
  titlePanel("Medidas dispersion"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons(inputId="n",
                   label = "Tipos de Datos",
                   choices = c('Ejemplos del libro','Generados aleatoriamente','Cargados'),
                   selected = " "),
      conditionalPanel( condition = "input.n=='Ejemplos del libro'",
                        selectInput( inputId = "m", 
                                     label = "Ejemplo",
                                     choices= c('Tiempo de uso de equipos','Sueldos','Otros'), 
                                     selected = NULL),
                        selectInput( inputId = "dispersion1", 
                                     label = "Elija medida de dispersión",
                                     choices= c('Desviación absoluta','Varianza','Desviación estándar','Rango'), 
                                     selected = NULL)
      ),
      conditionalPanel( condition = "input.n=='Cargados'",
                        fileInput( inputId = "datoscargados",
                                   label = "Seleccionar archivo:", buttonLabel = "Buscar...",
                                   placeholder = "Aun no seleccionas el archivo..."),
                        numericInput( inputId = "columna", 
                                      label="Elija el número de columna deseado", 
                                      min = 1, 
                                      max = 100,
                                      step = 1, 
                                      value = 1, 
                                      width = "100%"),
                        selectInput( inputId = "dispersion2", 
                                     label = "Elija medida de dispersión",
                                     choices= c('Desviación absoluta','Varianza','Desviación estándar','Rango'), 
                                     selected = NULL)
      ),
      conditionalPanel( condition = "input.n=='Generados aleatoriamente'",
                        sliderInput(inputId = "CantidadDatos",
                                    label = "Cantidad de datos a generar",
                                    min = 1,
                                    max = 100,
                                    value = 5),
                        selectInput( inputId = "dispersion3", 
                                     label = "Elija medida de dispersión",
                                     choices= c('Desviación absoluta','Varianza','Desviación estándar','Rango'), 
                                     selected = NULL)
      )
      
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
  
  output$texto<-renderPrint({
    if(is.null(input$n)){
      return()
    }
    else if(input$n=='Cargados'){
      ncolumna<-input$columna
      if(is.null(input$datoscargados)){
        return()
      }
      else if(input$dispersion2=='Desviación absoluta'){
        return(mean(abs(dat()[,ncolumna]-mean(dat()[,ncolumna]))))
      }
      else if(input$dispersion2=='Varianza'){
        return(var(dat()[,ncolumna]))
      }
      else if(input$dispersion2=='Desviación estándar'){
        return(sd(dat()[,ncolumna]))
      }
      else if(input$dispersion2=='Rango'){
        m<-range(dat()[,ncolumna])
        return(diff(m))
      }
    }
    else if(input$n=='Ejemplos del libro'){
      if(input$dispersion1=='Desviación absoluta'){
        return(mean(abs(dat()[,1]-mean(dat()[,1]))))
      }
      else if(input$dispersion1=='Varianza'){
        return(var(dat()[,1]))
      }
      else if(input$dispersion1=='Desviación estándar'){
        return(sd(dat()[,1]))
      }
      else if(input$dispersion1=='Rango'){
        m<-range(dat()[,1])
        return(diff(m))
      }
    }
    else if(input$n=='Generados aleatoriamente'){
      if(input$dispersion3=='Desviación absoluta'){
        return(mean(abs(dat()[,1]-mean(dat()[,1]))))
      }
      else if(input$dispersion3=='Varianza'){
        return(var(dat()[,1]))
      }
      else if(input$dispersion3=='Desviación estándar'){
        return(sd(dat()[,1]))
      }
      else if(input$dispersion3=='Rango'){
        m<-range(dat()[,1])
        return(diff(m))
      }
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
