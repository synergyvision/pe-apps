library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(modeest)

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
                                     min = 1, max = 20, value = 5),
                        sliderInput( "f",
                                     label = "Número de variables:",
                                     min = 1, max = 10, value = 5)
       ),
      conditionalPanel( condition = "input.n == 'car'", 
                        fileInput(inputId = "datoscargados",label = "Seleccionar desde un archivo guardado:", 
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo...")
      ),
      
      conditionalPanel( condition = "input.n == 'ejem'", 
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo:",
                                     choices= c("Sueldos","Horas","Ventas"), 
                                     selected = NULL)
      ),
      
      selectInput( inputId = "medias", label = "Medidas de Tendencia Central:",
                   choices= c("Media Aritmética","Media Geométrica","Media Armónica",
                              "Media Ponderada","Mediana","Moda"), 
                   selected = NULL)
      
    ),
    mainPanel(
       tableOutput("table"),
       verbatimTextOutput("medias")
    )
  )
)



server <- function(input, output) {

 data<-reactive ({
   if (is.null(input$n)){
     return(NULL)
   }
   
   else if(input$n=="gen"){
    M<-matrix( as.integer(runif(input$m*input$f,1,20)),
               ncol = input$f, nrow = input$m)
    colnames(M)<-c(paste0("Vari_",1:input$f))
    rownames(M)<-c(paste0("fil_",1:input$m))
    return(M)
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
  
 output$table <- renderTable({ data() },
                              striped = TRUE,hover = TRUE,
                              bordered = TRUE,rownames = FALSE)
 output$medias<-renderPrint({
   
   if(input$medias=="Media Aritmética"){
     colMeans(data())
   } else if(input$medias=="Media Geométrica"){
     geometric.mean(data())
   } else if(input$medias=="Media Armónica"){
     harmonic.mean(data())
   } else if(input$medias=="Mediana"){
     apply(data(),2,median)
   } else if(input$medias=="Moda"){
     apply(data(),2,mfv)
   }
   
 })
                             
 
}


shinyApp(ui = ui, server = server)
