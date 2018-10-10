ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("ggplot2", "3.0.0")
ensure_version("readxl", "1.1.0")

library(shiny)
library(ggplot2)
library(readxl)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Histogramas"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons(inputId="n",
                   label = "Tipos de Datos",
                   choices = c('Ejemplos del libro','Generados aleatoriamente','Cargados'),
                   selected = " "),
      conditionalPanel( condition = "input.n=='Ejemplos del libro'",
                        selectInput( inputId = "m", 
                                     label = "Ejemplo",
                                     choices= c('Sueldos','Ventas','Otros'), 
                                     selected = NULL),
                        radioButtons(inputId="interval",
                                     label = "Elección de intervalos de clases:",
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
                                                     label = "Número de intervalos:",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                                         ),
                        selectInput( inputId = "n1", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia absoluta','Frecuencia relativa'), 
                                     selected = NULL)
                        ),
      conditionalPanel( condition = "input.n=='Cargados'",
                        fileInput( inputId = "datoscargados",
                                   label = "Seleccionar archivo:", buttonLabel = "Buscar...",
                                   placeholder = "Aun no seleccionas el archivo..."),
                        numericInput( inputId = "columna", 
                                      label="Elija el número de columna deseado", 
                                      min = 1, 
                                      max = 100,
                                      step = 1, 
                                      value = 1, 
                                      width = "100%"),
                        radioButtons(inputId="interval1",
                                     label = "Elección de intervalos de clases:",
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
                                                     label = "Número de intervalos:",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                        ),
                        selectInput( inputId = "n2", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia absoluta','Frecuencia relativa'), 
                                     selected = NULL)
      ),
      conditionalPanel( condition = "input.n=='Generados aleatoriamente'",
                        sliderInput(inputId = "CantidadDatos",
                                    label = "Cantidad de datos a generar",
                                    min = 2,
                                    max = 100,
                                    value = 5),
                        radioButtons(inputId="interval2",
                                     label = "Elección de intervalos de clases:",
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
                                                     label = "Número de intervalos:",
                                                     min = 1,
                                                     max = 20,
                                                     value = 1)
                        ),
                        selectInput( inputId = "n3", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia absoluta','Frecuencia relativa'), 
                                     selected = NULL)
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      column(width=4,tableOutput("table")),
      column(width=8,plotOutput(outputId = "distPlot"))
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  Sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  Ventas<-c(rep(1,4),rep(2,5),rep(3,2),rep(4,10),rep(5,9),rep(6,6),rep(7,6))
  
  Otros<-c(rep(10,4),rep(22,5),rep(35,2),rep(46,10),rep(57,9),rep(68,6),rep(74,6))
  
  
  dat<-reactive({
    
    infile <- input$n
    if(is.null(infile)){
      return()
      }
    
    else if(infile=='Ejemplos del libro'){
      
      infile1<-input$m
      
      if(infile1=='Sueldos'){
       data.frame(Sueldos)
      }
      
      else if(infile1=='Ventas'){
        data.frame(Ventas)
      }
      
      else if(infile1=='Otros')
        data.frame(Otros)
    }
    
    else if(infile=='Cargados'){
      infile2<-input$datoscargados
      if(is.null(infile2)){
        return()
      }
      
      else{
        read_excel(infile2$datapath)
      }
    }
    
    else if(infile=='Generados aleatoriamente'){
      data.frame(Datos=sample(80:100,input$CantidadDatos,replace = TRUE))
    }
    
    })
  
  output$table<-renderTable({
    return(dat())
  },digits = 1)
  
  output$distPlot <- renderPlot({
    
    infile <- input$n
    
    if(is.null(infile)){
      return()
    }
    
    else if(infile=='Ejemplos del libro'){
      
      if(is.null(input$interval)){
        return()
      }
      
      else if(input$interval=='Métodos dados'){

      intervalo<-if(input$metodo1=='Fórmula de Sturges'){
      nclass.Sturges(dat()[,1])
      }
      else if(input$metodo1=='Regla de Scott'){
      nclass.scott(dat()[,1])
      }
      else if(input$metodo1=='Selección de Freedman-Diaconis'){
      nclass.FD(dat()[,1])
      }

      Ancho<-(max(dat()[,1])-min(dat()[,1]))/intervalo
      Centro<-(2*min(dat()[,1])+Ancho)/2

      if(input$n1=='Frecuencia absoluta'){

        ggplot(dat(),aes(x=dat()[,1]))+
          geom_histogram(aes(y=..count..),
                         closed="left",bins = intervalo,
                         fill="blue",col="black",alpha=0.7,binwidth = Ancho,center=Centro)+
          labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()[,1]),max(dat()[,1])+Ancho,by=Ancho))

      }

      else if(input$n1=='Frecuencia relativa'){

        ggplot(dat(),aes(x=dat()[,1]))+
          geom_histogram( aes(y=..density..),
                          closed="left",bins = intervalo,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho,center=Centro)+
          labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()[,1]),max(dat()[,1])+Ancho,by=Ancho))

      }


      }

      else if(input$interval=='Manual'){

       intervalo<-input$bins1

      Ancho<-(max(dat()[,1])-min(dat()[,1]))/intervalo
      Centro<-(2*min(dat()[,1])+Ancho)/2

      if(input$n1=='Frecuencia absoluta'){

        ggplot(dat(),aes(x=dat()[,1]))+
          geom_histogram(aes(y=..count..),
                         closed="left",bins = intervalo,
                         fill="blue",col="black",alpha=0.7,binwidth = Ancho,center=Centro)+
          labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()[,1]),max(dat()[,1])+Ancho,by=Ancho))

      }

      else if(input$n1=='Frecuencia relativa'){

        ggplot(dat(),aes(x=dat()[,1]))+
          geom_histogram( aes(y=..density..),
                          closed="left",bins = intervalo,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho,center=Centro)+
          labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()[,1]),max(dat()[,1])+Ancho,by=Ancho))


       }
      }

      
    }
    
    else if(infile=='Generados aleatoriamente'){
      
      if(is.null(input$interval2)){
        return()
      }
      
      else if(input$interval2=='Métodos dados'){
        
        intervalo1<-if(input$metodo3=='Fórmula de Sturges'){
          nclass.Sturges(dat()$Datos)
        }
        else if(input$metodo3=='Regla de Scott'){
          nclass.scott(dat()$Datos)
        }
        else if(input$metodo3=='Selección de Freedman-Diaconis'){
          nclass.FD(dat()$Datos)
        }
        
        Ancho1<-(max(dat()$Datos)-min(dat()$Datos))/intervalo1
        Centro1<-(2*min(dat()$Datos)+Ancho1)/2
        
        if(input$n3=='Frecuencia absoluta'){
          
          ggplot(dat(),aes(x=dat()$Datos))+
            geom_histogram( aes(y=..count..),
                            closed="left",bins = intervalo1,
                            fill="blue",col="black",alpha=0.7,binwidth = Ancho1,center=Centro1)+
            labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()$Datos),max(dat()$Datos)+Ancho1,by=Ancho1))
          
        }
        
        else if(input$n3=='Frecuencia relativa'){
          
          ggplot(dat(),aes(x=dat()$Datos))+
            geom_histogram( aes(y=..density..),
                            closed="left",bins = intervalo1,
                            fill="blue",col="black",alpha=0.7,binwidth = Ancho1,center=Centro1)+
            labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()$Datos),max(dat()$Datos)+Ancho1,by=Ancho1))
          
        }
        
        
      }
      
      else if(input$interval2=='Manual'){
      
      intervalo1<-input$bins3
      Ancho1<-(max(dat()$Datos)-min(dat()$Datos))/intervalo1
      Centro1<-(2*min(dat()$Datos)+Ancho1)/2

      if(input$n3=='Frecuencia absoluta'){

        ggplot(dat(),aes(x=dat()$Datos))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = intervalo1,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho1,center=Centro1)+
          labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()$Datos),max(dat()$Datos)+Ancho1,by=Ancho1))

      }

      else if(input$n3=='Frecuencia relativa'){

        ggplot(dat(),aes(x=dat()$Datos))+
          geom_histogram( aes(y=..density..),
                          closed="left",bins = intervalo1,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho1,center=Centro1)+
          labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()$Datos),max(dat()$Datos)+Ancho1,by=Ancho1))

      }
      
      }
    }
    
    else if(infile=='Cargados'){
      
      if(is.null(input$datoscargados)){
        return()
      }
      
      else{
        
        ncolumna<-input$columna
        
        if(is.null(input$interval1)){
          return()
        }
        
        else if(input$interval1=='Métodos dados'){
          
          intervalo2<-if(input$metodo2=='Fórmula de Sturges'){
            nclass.Sturges(dat()[,ncolumna][[1]])
          }
          else if(input$metodo2=='Regla de Scott'){
            nclass.scott(dat()[,ncolumna][[1]])
          }
          else if(input$metodo2=='Selección de Freedman-Diaconis'){
            nclass.FD(dat()[,ncolumna][[1]])
          }
          
          Ancho2<-(max(dat()[,ncolumna][[1]])-min(dat()[,ncolumna][[1]]))/intervalo2
          Centro2<-(2*min(dat()[,ncolumna][[1]])+Ancho2)/2
          
          if(input$n2=='Frecuencia absoluta'){
            
            ggplot(dat(),aes(x=dat()[,ncolumna][[1]]))+
              geom_histogram( aes(y=..count..),
                              closed="left",bins = intervalo2,
                              fill="blue",col="black",alpha=0.7,binwidth = Ancho2,center=Centro2)+
              labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()[,ncolumna][[1]]),max(dat()[,ncolumna][[1]])+Ancho2,by=Ancho2))
            
          }
          
          else if(input$n2=='Frecuencia relativa'){
            
            ggplot(dat(),aes(x=dat()[,ncolumna][[1]]))+
              geom_histogram( aes(y=..density..),
                              closed="left",bins = intervalo2,
                              fill="blue",col="black",alpha=0.7,binwidth = Ancho2,center=Centro2)+
              labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()[,ncolumna][[1]]),max(dat()[,ncolumna][[1]])+Ancho2,by=Ancho2))
          }
          
        }
        
        
        else if(input$interval1=='Manual'){
        
        intervalo2<-input$bins2
        Ancho2<-(max(dat()[,ncolumna][[1]])-min(dat()[,ncolumna][[1]]))/intervalo2
        Centro2<-(2*min(dat()[,ncolumna][[1]])+Ancho2)/2
      
      if(input$n2=='Frecuencia absoluta'){

        ggplot(dat(),aes(x=dat()[,ncolumna][[1]]))+
          geom_histogram( aes(y=..count..),
                          closed="left",bins = intervalo2,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho2,center=Centro2)+
          labs(title = "Histograma", x="Clases", y="Frecuencia")+scale_x_continuous(breaks = seq(min(dat()[,ncolumna][[1]]),max(dat()[,ncolumna][[1]])+Ancho2,by=Ancho2))

      }

      else if(input$n2=='Frecuencia relativa'){

        ggplot(dat(),aes(x=dat()[,ncolumna][[1]]))+
          geom_histogram( aes(y=..density..),
                          closed="left",bins = intervalo2,
                          fill="blue",col="black",alpha=0.7,binwidth = Ancho2,center=Centro2)+
          labs(title = "Histograma", x="Clases", y="Frecuencia Relativa")+scale_x_continuous(breaks = seq(min(dat()[,ncolumna][[1]]),max(dat()[,ncolumna][[1]])+Ancho2,by=Ancho2))
      }
        }
      }

    } 


    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
