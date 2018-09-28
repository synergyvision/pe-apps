library(shiny)

# Crear tablas de frecuencias con Sueldos.
ui <- fluidPage(

  titlePanel("Tablas de Frecuencias"),

  sidebarLayout(

    sidebarPanel(
      
      selectInput( inputId = "n", label = "Opciones:",choices= c("Frecuencia","Frecuencia Relativa"), selected = NULL
                   ), 

      sliderInput(inputId = "bins",
                  label = "Número de intervalos:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
       mainPanel(
         
              tableOutput(outputId = "table")
    )
  )
)


server <- function(input, output) {

  
  output$table <- renderTable({

    x <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
                         54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
    
    Intervalo <- cut(x,breaks = seq(min(x),max(x),length.out = input$bins+1),include.lowest = T,right = F)
    
    #conteo
    if(input$n=="Frecuencia"){
    conteo<-table(Intervalo)
    df<-data.frame(conteo)
    colnames(df)<-c("Intervalos","Frecuencia")
    return(df)
    }
    else if(input$n=="Frecuencia Relativa"){
    conteo<-table(Intervalo)
    df<-data.frame(conteo)
    colnames(df)<-c("Intervalos","Frecuencia")
    fr<-transform(df,Fr=prop.table(Frecuencia))
    colnames(fr)<-c("Intervalos","Frecuencias","Frecuencias Relativa")
    return(fr)
    }

    })

}


shinyApp(ui = ui, server = server)
