library(shiny)

# Crear polígonos de frecuencias con Sueldos.
ui <- fluidPage(

  titlePanel("Diagrama de Tallo y Hoja"),

  sidebarLayout(

    sidebarPanel(

      sliderInput(inputId = "scale",
                  label = "Número de escala:",
                  min = 0.2,
                  max = 2,
                  value = 0.2,
                  step = 0.1),
      verbatimTextOutput("prueba")
      ),
    
    
       mainPanel(
         
              verbatimTextOutput("displot")
    )
  )
)


server <- function(input, output) {

  sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  output$prueba<-renderPrint({
    input$scale
  })
  
  output$displot <- renderPrint({
       
  stem(x=sueldos, scale = input$scale)
    

    })

}


shinyApp(ui = ui, server = server)
