ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("ggplot2", "3.0.0")


library(shiny)
library(ggplot2)
library(readxl)

# Crear polígonos de frecuencias.
ui <- fluidPage(

  titlePanel("Polígonos de frecuencia"),

  sidebarLayout(

    sidebarPanel(
      radioButtons( inputId = "n",label = "Origen de los datos",
                    choices = c("Generados"="gen", "Cargados"="car", "Ejemplos"="ejem"),
                    selected=" "),
      conditionalPanel( condition = "input.n == 'gen'",
                        sliderInput( "m",
                                     label = "Cantidad de datos",
                                     min = 2, max = 50, value = 5),
                        radioButtons(inputId="interval",
                                     label = "Elección de intervalos de clases",
                                     choices = c('Métodos dados','Manual'),
                                     selected = " "),
                        conditionalPanel(condition = "input.interval=='Métodos dados'",
                                         selectInput( inputId = "metodo1",
                                                      label = "Elija el método a usar",
                                                      choices= c('Fórmula de Sturges','Regla de Scott','Selección de Freedman-Diaconis'),
                                                      selected = NULL)
                        ),
                        conditionalPanel(condition = "input.interval=='Manual'",
                                         sliderInput(inputId = "bins1",
                                                     label = "Número de intervalos",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                        )


      ),
      conditionalPanel( condition = "input.n == 'car'",
                        fileInput(inputId = "datoscargados",label = "Seleccionar desde un archivo guardado",
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo..."),
                        numericInput( inputId = "d", label="Escoja el número de columna deseado", min = 1,
                                      max = 100,step = 1,
                                      value = 1, width = "100%"),
                        radioButtons(inputId="interval1",
                                     label = "Elección de intervalos de clases",
                                     choices = c('Métodos dados','Manual'),
                                     selected = " "),
                        conditionalPanel(condition = "input.interval1=='Métodos dados'",
                                         selectInput( inputId = "metodo2",
                                                      label = "Elija el método a usar",
                                                      choices= c('Fórmula de Sturges','Regla de Scott','Selección de Freedman-Diaconis'),
                                                      selected = NULL)
                        ),
                        conditionalPanel(condition = "input.interval1=='Manual'",
                                         sliderInput(inputId = "bins2",
                                                     label = "Número de intervalos",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                        )



      ),

      conditionalPanel( condition = "input.n == 'ejem'",
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo",
                                     choices= c("Sueldos","Horas","Ventas"),
                                     selected = NULL),
                        radioButtons(inputId="interval2",
                                     label = "Elección de intervalos de clases",
                                     choices = c('Métodos dados','Manual'),
                                     selected = " "),
                        conditionalPanel(condition = "input.interval2=='Métodos dados'",
                                         selectInput( inputId = "metodo3",
                                                      label = "Elija el método a usar",
                                                      choices= c('Fórmula de Sturges','Regla de Scott','Selección de Freedman-Diaconis'),
                                                      selected = NULL)
                        ),
                        conditionalPanel(condition = "input.interval2=='Manual'",
                                         sliderInput(inputId = "bins3",
                                                     label = "Número de intervalos",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                        )





      )

    ),
       mainPanel(
              tabsetPanel(type = 'tabs',id='p',
              tabPanel('Datos',br(),dataTableOutput(outputId = "table")),
              tabPanel('Polígono de frecuencia',br(),br(),column(width=12,align='center',plotOutput(outputId = "distPlot"))))
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
      else{

      read_excel(file1$datapath)
      }


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

  output$table<- renderDataTable({data()},
                             options = list(scrollX=TRUE,scrollY=300,searching=FALSE))


  output$distPlot <- renderPlot({

    if(is.null(input$n)||is.null(data())){
      return()
    }


    else if(input$n=="gen"){

      if(is.null(input$interval)){
        return()
      }

      else if(input$interval=="Métodos dados"){


        k<-if(input$metodo1=='Fórmula de Sturges'){
          nclass.Sturges(data()[,1])
        }
        else if(input$metodo1=='Regla de Scott'){
          nclass.scott(data()[,1])
        }
        else if(input$metodo1=='Selección de Freedman-Diaconis'){
          nclass.FD(data()[,1])
        }

        Ancho<-(max(data()[,1])-min(data()[,1]))/k
        Centro<-(2*min(data()[,1])+Ancho)/2


        ggplot(data(),aes(x=data()[,1]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = k,linetype="dashed",
                          fill="blue",col="black",alpha=0.3,binwidth = Ancho,center=Centro,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho))+
          geom_freqpoly(closed='left',col="red",size=0.8,bins=k,binwidth = Ancho,center=Centro)+
          labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")

         } else if(input$interval=="Manual"){

           k1<-input$bins1

           Ancho1<-(max(data()[,1])-min(data()[,1]))/k1
           Centro1<-(2*min(data()[,1])+Ancho1)/2

           ggplot(data(),aes(x=data()[,1]))+
             geom_histogram( aes(y=..count..),
                             closed="left",bins = k1,linetype="dashed",
                             fill="blue",col="black",alpha=0.3,binwidth = Ancho1,center=Centro1,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho1))+
             geom_freqpoly(closed='left',col="red",size=0.8,bins=k1,binwidth = Ancho1,center=Centro1)+
             labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")

      }

    } else if(input$n=="car"){

      ncol<-input$d

      if(is.null(input$interval1)){
        return()
      }

      else if(input$interval1=="Métodos dados"){
        k2<-if(input$metodo2=='Fórmula de Sturges'){
          nclass.Sturges(data()[,ncol][[1]])
        }
        else if(input$metodo2=='Regla de Scott'){
          nclass.scott(data()[,ncol][[1]])
        }
        else if(input$metodo2=='Selección de Freedman-Diaconis'){
          nclass.FD(data()[,ncol][[1]])
        }

        Ancho2<-(max(data()[,ncol][[1]])-min(data()[,ncol][[1]]))/k2
        Centro2<-(2*min(data()[,ncol][[1]])+Ancho2)/2


        ggplot(data(),aes(x=data()[,ncol][[1]]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = k2,linetype="dashed",
                          fill="blue",col="black",alpha=0.3,binwidth = Ancho2,center=Centro2,breaks=seq(min(data()[,ncol][[1]]),max(data()[,ncol][[1]]),by=Ancho2))+
          geom_freqpoly(closed='left',col="red",size=0.8,bins=k2,binwidth = Ancho2,center=Centro2)+
          labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")


      } else if(input$interval1=="Manual"){
        k3<-input$bins2

        Ancho3<-(max(data()[,ncol][[1]])-min(data()[,ncol][[1]]))/k3
        Centro3<-(2*min(data()[,ncol][[1]])+Ancho3)/2

        ggplot(data(),aes(x=data()[,ncol][[1]]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = k3,linetype="dashed",
                          fill="blue",col="black",alpha=0.3,binwidth = Ancho3,center=Centro3,breaks=seq(min(data()[,ncol][[1]]),max(data()[,ncol][[1]]),by=Ancho3))+
          geom_freqpoly(closed='left',col="red",size=0.8,bins=k3,binwidth = Ancho3,center=Centro3)+
          labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")

      }



    } else if(input$n=="ejem"){

      if(is.null(input$interval2)){
        return()
      }

      else if(input$interval2=="Métodos dados"){

        k4<-if(input$metodo3=='Fórmula de Sturges'){
          nclass.Sturges(data()[,1])
        }
        else if(input$metodo3=='Regla de Scott'){
          nclass.scott(data()[,1])
        }
        else if(input$metodo3=='Selección de Freedman-Diaconis'){
          nclass.FD(data()[,1])
        }

        Ancho4<-(max(data()[,1])-min(data()[,1]))/k4
        Centro4<-(2*min(data()[,1])+Ancho4)/2


        ggplot(data(),aes(x=data()[,1]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = k4,linetype="dashed",
                          fill="blue",col="black",alpha=0.3,binwidth = Ancho4,center=Centro4,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho4))+
          geom_freqpoly(closed='left',col="red",size=0.8,bins=k4,binwidth = Ancho4,center=Centro4,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho4))+
          labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")


      } else if(input$interval2=="Manual"){
        k5<-input$bins3

        Ancho5<-(max(data()[,1])-min(data()[,1]))/k5
        Centro5<-(2*min(data()[,1])+Ancho5)/2

        ggplot(data(),aes(x=data()[,1]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = k5,linetype="dashed",
                          fill="blue",col="black",alpha=0.3,binwidth = Ancho5,center=Centro5,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho5))+
          geom_freqpoly(closed='left',col="red",size=0.8,bins=k5,binwidth = Ancho5,center=Centro5,breaks=seq(min(data()[,1]),max(data()[,1]),by=Ancho5))+
          labs(title = "Polígono de frecuencia", x="Clases", y="Frecuencia",caption = "https://synergy.vision/")

      }


}



    })

}


shinyApp(ui = ui, server = server)
