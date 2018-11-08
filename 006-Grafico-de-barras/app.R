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
  titlePanel("Gráficos de Barras"),

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
                                      width = "100%")
                        ),
      conditionalPanel( condition = "input.n=='Generados aleatoriamente'",
                        sliderInput(inputId = "CantidadDatos",
                                    label = "Cantidad de datos a generar",
                                    min = 1,
                                    max = 100,
                                    value = 5)
                      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput('tabla'),
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)

  Horas<-c(2,3,4,6,7)
  Frecuencia<-c(46,15,12,52,8)
  
  output$tabla<-renderTable({
    if(input$n=='Tiempo de uso de equipos'){
      data<-data.frame(Horas,Frecuencia)
      return(data)
    }
    else if(input$n=='Sueldos'){
      fr<-data.frame(table(sueldos))
      colnames(fr)<-c("Sueldos","Frecuencia")
      return(fr)
    }
  },digits = 0)
  
  output$distPlot<-renderPlot({
    if(input$n=='Tiempo de uso de equipos'){
      
      data<-data.frame(Horas,Frecuencia)
      
      ggplot(data, aes(x=Horas,y=Frecuencia))+
        geom_bar(stat = "identity", color="black",
                 fill="Blue", alpha=0.5)+
        labs(title = "Diagrama de Barra", x="Horas",y="Frecuencias")
    }
    else if(input$n=='Sueldos'){
      fr<-data.frame(table(sueldos))
      colnames(fr)<-c("Sueldos","Frecuencia")
      
      ggplot(fr, aes(x=Sueldos,y=Frecuencia))+
        geom_bar(stat = "identity", color="black",
                 fill="Blue", alpha=0.5)+
        labs(title = "Diagrama de Barra", 
             x="Sueldos en miles de dólares",y="Frecuencias")
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
