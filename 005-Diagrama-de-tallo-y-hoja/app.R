library(shiny)

# Crear diagramas de tallo y hoja con Sueldos.
ui <- fluidPage(

  titlePanel("Diagrama de Tallo y Hoja"),

  sidebarLayout(

    sidebarPanel(
      
      radioButtons( inputId = "n",label = "Origen de los datos:", 
                    choices = c("Generados"="gen", "Cargados"="car", "Ejemplos"="ejem"),
                    selected=" "),
      conditionalPanel( condition = "input.n == 'gen'", 
                        sliderInput( "m",
                                     label = "Cantidad de datos:",
                                     min = 1, max = 50, value = 5),
                        sliderInput(inputId = "scale",
                       label = "Número de escala:",
                       min = 0.2,
                       max = 2,
                       value = 0.2,
                       step = 0.1)),
      conditionalPanel( condition = "input.n == 'car'", 
                        fileInput(inputId = "datoscargados",label = "Seleccionar desde un archivo guardado:", 
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo..."),
                        numericInput( inputId = "d", label="Escoja el número de columna deseado:", min = 1,
                                      max = 100,step = 1,
                                      value = 1, width = "40%"),
                        sliderInput(inputId = "scale1",
                                    label = "Número de escala:",
                                    min = 0.2,
                                    max = 2,
                                    value = 0.2,
                                    step = 0.1)),
      conditionalPanel( condition = "input.n == 'ejem'", 
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo:",
                                     choices= c("Sueldos","Horas","Ventas"), 
                                     selected = NULL),
                        sliderInput(inputId = "scale1",
                                    label = "Número de escala:",
                                    min = 0.2,
                                    max = 2,
                                    value = 0.2,
                                    step = 0.1))
                        
                        
      ),
    
    
       mainPanel(
         
         column(width=4,div(style="height:400px; overflow-y: scroll",tableOutput(outputId = "table"))),
              column(width=8,verbatimTextOutput("displot"))
    )
  )
)


server <- function(input, output) {

  sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)

  
  output$displot <- renderPrint({
       
  stem(x=sueldos, scale = as.numeric(input$scale))
    

    })

}


shinyApp(ui = ui, server = server)
