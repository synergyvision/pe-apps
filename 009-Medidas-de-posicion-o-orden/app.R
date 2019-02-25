ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydashboard", "0.7.0")
ensure_version("psych", "1.8.4")
# ensure_version("modeest", "2.1")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.0.0")

library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
# library(modeest)
library(matrixStats)
library(ggplot2)

ui <- fluidPage(

  titlePanel("Medidas de posición u orden"),

  sidebarLayout(

    sidebarPanel(

      radioButtons( inputId = "n",label = "Origen de los datos",
                    choices = c("Generados"="gen", "Cargados"="car", "Ejemplos"="ejem"),
                    selected=" "),
      conditionalPanel( condition = "input.n == 'gen'",
                         sliderInput( "m",
                                     label = "Número de filas",
                                     min = 1, max = 20, value = 5),
                        sliderInput( "f",
                                     label = "Número de variables",
                                     min = 1, max = 10, value = 5)

       ),
      conditionalPanel( condition = "input.n == 'car'",
                        fileInput(inputId = "datoscargados",label = "Seleccionar desde un archivo guardado",
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo...")

      ),

      conditionalPanel( condition = "input.n == 'ejem'",
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo",
                                     choices= c("Sueldos","Horas","Ventas"),
                                     selected = NULL)),
          selectInput( inputId = "medias", label = "Medidas de posición o orden",
                       choices= c("Cuantiles","Diagrama de caja"
                                                ),
                        selected = NULL),
      conditionalPanel(condition = "input.medias== 'Cuantiles'",textInput(inputId = "vect", label = "Introducir la probabilidad deseada",
                                                                          placeholder = "0.1,0.2,...")),
      conditionalPanel(condition= "input.medias=='Diagrama de caja'",
                       conditionalPanel(condition= "input.n=='gen'",
                                        numericInput( inputId = "k", label="Escoja el número de la variable deseada", min = 1,
                                                      max = 100,step = 1,
                                                      value = 1, width = "40%")),
                       conditionalPanel(condition="input.n=='car'",
                                        numericInput( inputId = "d", label="Escoja el número de columna deseado", min = 1,
                                                      max = 100,step = 1,
                                                      value = 1, width = "40%")))


    ),
    mainPanel(

      tabsetPanel(type = 'tabs',id='o',
      tabPanel('Datos',dataTableOutput("table")),

      tabPanel('Medidas de posición u orden',
      conditionalPanel(condition ="input.medias=='Cuantiles'",column(width=12,h3("Cuantiles"),verbatimTextOutput("cuantiles"))),
      conditionalPanel(condition ="input.medias=='Diagrama de caja'",column(width=12,h3("Diagrama de caja"),plotOutput("plot")))))
    )
  )
)



server <- function(input, output) {

 data<-reactive ({
   if (is.null(input$n)){
     return()
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


 output$table <- renderDataTable({ data() },
                                 options = list(scrollX=TRUE,scrollY=300,searching=FALSE))



 output$cuantiles<-renderPrint({

   if(is.null(input$medias)||is.null(data())){
     return(print('Introduzca los datos'))
   }

   else if(input$medias=="Cuantiles"){

      if(is.null(input$vect)){
        return(print('Introduzca la probabilidad asociada al cuantil'))
       } else{
       x <- as.numeric(unlist(strsplit(input$vect,",")))

       apply(data(),2,quantile,probs = x)

       }

   }
})

   output$plot<-renderPlot({

     if(input$medias=="Diagrama de caja"){

       if(input$n=="gen"){
         datos<-as.data.frame(data())

         ncol<-input$k

         ggplot(datos, aes(x=length(datos[,ncol]),y=datos[,ncol]))+
           geom_boxplot(fill="blue",alpha=0.5)+
           labs(title = "Diagrama de caja",
                x=colnames(datos[ncol]),y="")
       }

       else if(input$n=="car"){
       datos<-as.data.frame(data())

       ncol<-input$d

       ggplot(datos, aes(x=length(datos[,ncol]),y=datos[,ncol]))+
         geom_boxplot(fill="blue",alpha=0.5)+
         labs(title = "Diagrama de caja",
              x=colnames(datos[ncol]),y="")
       }
       else if(input$n=="ejem"){
         datos<-as.data.frame(data())

         ggplot(datos, aes(x=length(datos[,1]),y=datos[,1]))+
           geom_boxplot(fill="blue",alpha=0.5)+
           labs(title = "Diagrama de caja",
                x=colnames(datos[1]),y="")
       }

     }


   })


}


shinyApp(ui = ui, server = server)
