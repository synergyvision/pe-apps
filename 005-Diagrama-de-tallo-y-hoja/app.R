ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")

library(shiny)
library(readxl)

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
                        sliderInput(inputId = "scale2",
                                    label = "Número de escala:",
                                    min = 0.2,
                                    max = 2,
                                    value = 0.2,
                                    step = 0.1))
                        
                        
      ),
    
    
       mainPanel(
         
         column(width=4,div(style="height:400px; overflow-y: scroll",tableOutput(outputId = "table"))),
              column(width=8,verbatimTextOutput(outputId = "displot"))
    )
  )
)


server <- function(input, output) {
  
  
  data<-reactive ({
   if (is.null(input$n)){
      return()
   }

    else if(input$n=="gen"){
      data.frame(Datos=sample(80:100,input$m,replace = TRUE))
    } else if(input$n=="car"){
      
      file1<-input$datoscargados
      
      if(is.null(file1)){
        return()
      }
      
      read_excel(file1$datapath)
      
      
    } else if(input$n=="ejem"){
      
      Sueldos<- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
                  54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
      Horas<-c(rep(2,46),rep(3,15),rep(4,12),rep(6,52),rep(7,8))
      
      Ventas <- c(1034,1075,1123,1172,1218,1265,1313,1379,1452,1597)
      
      if(input$ejemplos=="Sueldos"){
        data.frame(Sueldos)
      } else if(input$ejemplos=="Horas"){
        data.frame(Horas)
      } else if(input$ejemplos=="Ventas"){
        data.frame(Ventas)
      }
      
    }
    
  })
  
  output$table<- renderTable({data()},
                             striped = TRUE,hover = TRUE,
                             bordered = TRUE,rownames = FALSE)
  

  
  
  output$displot <- renderPrint({
  
    if(is.null(input$n))({
      return()
    })
    
    
    num<-as.numeric(unlist(data()))
    scale3<-as.numeric(unlist(input$scale))
    scale1<-as.numeric(unlist(input$scale1))
    scale2<-as.numeric(unlist(input$scale2))

      if(input$n=="gen")({
        stem(x=num, scale = scale3)
      })
     else if(input$n=="car")({
       
       if(is.null(input$datoscargados))({
         return()
       }) else({
       stem(x=num, scale = scale1)
       })
     })
     else if(input$n=="ejem")({
       stem(x=num, scale = scale2)
     })

 invisible(NULL)

  })

}


shinyApp(ui = ui, server = server)
