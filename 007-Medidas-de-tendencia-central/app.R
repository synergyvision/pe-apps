ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydashboard", "0.7.0")
ensure_version("DescTools", "0.99.27")
ensure_version("psych", "1.8.4")
ensure_version("matrixStats", "0.54.0")

library(shiny)
library(shinydashboard)
library(readxl)
library(DescTools)
library(psych)
library(matrixStats)

ui <- fluidPage(

  titlePanel("Medidas de tendencia central"),

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
          selectInput( inputId = "medias", label = "Medidas de tendencia central",
                       choices= c("Media aritmética","Media geométrica","Media armónica",
                                                "Media ponderada","Mediana","Moda"),
                        selected = NULL)


    ),
    mainPanel(

      div(style="height:400px; overflow-y: scroll",tableOutput("table"))
      # div(style="height:100px; overflow-y: scroll",verbatimTextOutput("medias1"))
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
    rownames(M)<-c(paste0(" ",1:input$m))
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


 output$table <- renderTable({

   if(is.null(input$medias)||is.null(data())){
     return()
   }

   if(input$medias=="Media aritmética"){

     Media_aritmética<-apply(data(),2,mean)

     rbind(data(),Media_aritmética)
    } else if(input$medias=="Media geométrica"){

      Media_geométrica<-geometric.mean(data())

      rbind(data(),Media_geométrica)
    } else if(input$medias=="Media armónica"){

     Media_armónica<-harmonic.mean(data())
      rbind(data(),Media_armónica)

    } else if(input$medias=="Mediana"){

      Mediana<-apply(data(),2,median)
      rbind(data(),Mediana)

    } else if(input$medias=="Moda"){
      Moda<-apply(data(),2,Mode)
   rbind(data(),Moda)}

   # } else if(input$medias=="Media ponderada"){
   #
   #   if(input$n=="gen"){
   #     w<-prop.table(data(),2) #Pesos generados.
   #
   #     w1<-data()*w
   #     w1<-colSums(w1)
   #     return(w1)
   #   } else if(input$n=="car"){
   #
   #     d<-as.matrix(data())
   #     w2<-prop.table(d,2) #Pesos generados.
   #
   #     w3<-d*w2
   #     w3<-colSums(w3)
   #     return(w3)
   #
   #   } else if(input$n=="ejem"){
   #     d1<-as.matrix(data())
   #     w4<-prop.table(d1,2) #Pesos generados.
   #
   #     w5<-d1*w4
   #     w5<-colSums(w5)
   #     return(w5)
   #   }
   # }

 }, striped = TRUE,hover = TRUE,
                             bordered = TRUE,rownames = TRUE)

 # })

 # output$table <- renderTable({ data() },
 #                             striped = TRUE,hover = TRUE,
 #                             bordered = TRUE,rownames = FALSE)


}


shinyApp(ui = ui, server = server)
