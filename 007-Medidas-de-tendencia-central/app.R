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

    d<-read_excel(file1$datapath)
    d<-as.matrix(d)
    rownames(d)<-c(paste0(" ",1:nrow(d)))
    return(d)


   } else if(input$n=="ejem"){

     Sueldos<- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
                      54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
     Horas<-c(rep(2,46),rep(3,15),rep(4,12),rep(6,52),rep(7,8))

     Ventas <- c(1034,1075,1123,1172,1218,1265,1313,1379,1452,1597)

        if(input$ejemplos=="Sueldos"){
         m<-matrix(Sueldos)
         colnames(m)<-'Sueldos'
         rownames(m)<-c(paste0(" ",1:42))
         return(m)

        } else if(input$ejemplos=="Horas"){
          m1<-matrix(Horas)
          colnames(m1)<-'Horas'
          rownames(m1)<-c(paste0(" ",1:133))
          return(m1)

        } else if(input$ejemplos=="Ventas"){
             m2<-matrix(Ventas)
             colnames(m2)<-'Ventas'
             rownames(m2)<-c(paste0(" ",1:10))
             return(m2)
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
   rbind(data(),Moda)

    } else if(input$medias=="Media ponderada"){

      if(input$n=="gen"){
        w<-prop.table(data(),2) #Pesos generados.

        w1<-data()*w
        Media_ponderada<-colSums(w1)

        d<-rbind(data(),Media_ponderada)
        return(d)
      } else if(input$n=="car"){


        w2<-prop.table(data(),2) #Pesos generados.

        w3<-data()*w2
        Media_ponderada<-colSums(w3)
        d1<-rbind(data(),Media_ponderada)
        return(d1)

      } else if(input$n=="ejem"){

       w4<-prop.table(data(),2) #Pesos generados.

        w5<-data()*w4
        Media_ponderada<-colSums(w5)
        d2<-rbind(data(),Media_ponderada)
        return(d2)
      }
 }

 }, striped = TRUE,hover = TRUE,
                             bordered = TRUE,rownames = TRUE)

}


shinyApp(ui = ui, server = server)
