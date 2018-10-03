library(shiny)
library('ggplot2')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Histogramas"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons(inputId="n",
                   label = "Tipos de Datos",
                   choices = c('Ejemplos del libro','Generados aleatoriamente','cargados'),
                   selected = " "),
      conditionalPanel( condition = "input.n=='Ejemplos del libro'",
                        selectInput( inputId = "m", 
                                     label = "Ejemplo",
                                     choices= c('Sueldos','Ventas','Otros'), 
                                     selected = NULL),
                        selectInput( inputId = "n1", 
                                     label = "Ejemplo",
                                     choices= c('Frecuencia absoluta','Frecuencia relativa'), 
                                     selected = NULL),
                        sliderInput(inputId = "bins",
                                    label = "Número de intervalos:",
                                     min = 1,
                                     max = 10,
                                     value = 1)
                                         
                        ),
      conditionalPanel( condition = "input.n=='cargados'",
                        fileInput( inputId = "datoscargados",
                                   label = "Seleccionar archivo:", buttonLabel = "Buscar...",
                                   placeholder = "Aun no seleccionas el archivo...")
      ),
      conditionalPanel( condition = "input.n=='Generados aleatoriamente'",
                        sliderInput(inputId = "filas",
                                    label = "Número de filas",
                                    min = 1,
                                    max = 10,
                                    value = 1),
                        sliderInput(inputId = "columnas",
                                    label = "Número de columnas",
                                    min = 1,
                                    max = 10,
                                    value = 1)
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
         #plotOutput(outputId = "distPlot")
         tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  Sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  Ventas<-c(rep(1,4),rep(2,5),rep(3,2),rep(4,10),rep(5,9),rep(6,6),rep(7,6))
  
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
      
      else if(infile1=='Ventas'){
        data.frame(Ventas)
      }
      
      else if(infile1=='Otros')
        data.frame(Otros)
    }
    
    else if(infile=='cargados'){
      return()
    }
    
    else if(infile=='Generados aleatoriamente'){
      return()
    }
    
    })
  
  output$table<-renderTable({
    return(data.frame(dat()))
  })
  
  # output$distPlot <- renderPlot({
  #   
  #   if(input$n=='Frecuencia absoluta'){
  #   
  #   ggplot(dat,aes(x=sueldos))+
  #     geom_histogram( aes(y=..count..),
  #                     closed="left",bins = input$bins,
  #                     fill="blue",col="black",alpha=0.7)+
  #     labs(title = "Histograma", x="Clases", y="Frecuencia")
  #     
  #   }
  #   
  #   else if(input$n=='Frecuencia relativa'){
  #     
  #     ggplot(dat,aes(x=sueldos))+
  #       geom_histogram( aes(y=..density..),
  #                       closed="left",bins = input$bins,
  #                       fill="blue",col="black",alpha=0.7)+
  #       labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")
  #     
  #   }
  # 
  # 
  #   })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

#----------------------------------------------------------------------------------------------------
# selectInput( inputId = "n", 
#              label = "Tipo de frecuencia",
#              choices= c('Frecuencia absoluta','Frecuencia relativa'), 
#              selected = NULL),
# 
# sliderInput(inputId = "bins",
#             label = "Número de intervalos:",
#             min = 1,
#             max = 10,
#             value = 1)
