library(shiny)
library(ggplot2)

# Crear polígonos de frecuencias con Sueldos.
ui <- fluidPage(

  titlePanel("Polígonos de Frecuencias"),

  sidebarLayout(

    sidebarPanel(

      sliderInput(inputId = "bins",
                  label = "Número de intervalos:",
                  min = 1,
                  max = 50,
                  value = 8)
      
    ),
       mainPanel(
         
              plotOutput(outputId = "distPlot")
    )
  )
)


server <- function(input, output) {

  
  output$distPlot <- renderPlot({

  sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
    
    dat<-data.frame(sueldos)
    ggplot(dat=dat,aes(x=sueldos))+ 
      geom_histogram( aes(y=..count..),
                    closed="left",bins = input$bins,linetype="dashed",
                      fill="blue",col="black",alpha=0.3)+
      geom_freqpoly( col="red",size=0.8,bins=input$bins)+
      labs(title = "Polígono de Frecuencia", x="x", y="Frecuencia")
    
    })

}


shinyApp(ui = ui, server = server)
