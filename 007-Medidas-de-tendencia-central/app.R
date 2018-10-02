library(shiny)
library(shinydashboard)

ui <- fluidPage(
  
  titlePanel("Medidas de Tendencia Central"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons( inputId = "n",label = "Origen de los datos:", 
                    choices = c("Generados"="gen", "Cargados"="car", "Ejemplos"="ejem"),
                    selected=" "),
      conditionalPanel( condition = "input.n == 'gen'", 
                         sliderInput( "m",
                                     label = "Número de filas:",
                                     min = 1, max = 20, value = 1, step = 10),
                        sliderInput( "f",
                                     label = "Número de variables:",
                                     min = 1, max = 10, value = 1, step = 5)
       ),
      conditionalPanel( condition = "input.n == 'car'", 
                        fileInput(inputId = "datoscargados",label = "Seleccionar desde un archivo guardado:", 
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo...")
      ),
      
      conditionalPanel( condition = "input.n == 'ejem'", 
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo:",
                                     choices= c("Sueldos","Ventas","Datos"), 
                                     selected = NULL)
      )
      
    ),
    mainPanel(
       plotOutput("displot")
    )
  )
)



server <- function(input, output) {

  
}


shinyApp(ui = ui, server = server)
