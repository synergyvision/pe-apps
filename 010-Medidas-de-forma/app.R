ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("ggplot2", "3.0.0")
ensure_version("readxl", "1.1.0")
ensure_version('e1071','1.7-0')

library(shiny)
library(ggplot2)
library(readxl)
library(e1071)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Medidas de forma"),

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
                                     choices= c('Tiempo de uso de equipos','Sueldos','Otros'), 
                                     selected = NULL),
                        selectInput( inputId = "forma1", 
                                     label = "Elija medida de forma",
                                     choices= c('Sesgo','Curtosis'), 
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
                        selectInput( inputId = "forma2", 
                                     label = "Elija medida de forma",
                                     choices= c('Sesgo','Curtosis'), 
                                     selected = NULL)
      ),
      conditionalPanel( condition = "input.n=='Generados aleatoriamente'",
                        sliderInput(inputId = "CantidadDatos",
                                    label = "Cantidad de datos a generar",
                                    min = 1,
                                    max = 100,
                                    value = 5),
                        selectInput( inputId = "forma3", 
                                     label = "Elija medida de forma",
                                     choices= c('Sesgo','Curtosis'), 
                                     selected = NULL)
      )
      
      
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(column(width=4,div(style="height:400px; overflow-y: scroll",tableOutput("table"))),
      column(width=8,verbatimTextOutput(outputId = "texto"))),tags$br(),
      fluidRow(plotOutput('ploteo'))
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  Sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  Horas<-c(rep(2,46),rep(3,15),rep(4,12),rep(6,52),rep(7,8))
  
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
      
      else if(infile1=='Tiempo de uso de equipos'){
        data.frame(Horas)
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
        as.data.frame(read_excel(infile2$datapath))
      }
    }
    
    else if(infile=='Generados aleatoriamente'){
      data.frame(Datos=sample(80:100,input$CantidadDatos,replace = TRUE))
    }
    
    })
  
  output$table<-renderTable({
    return(dat())
  },digits = 1)
  
  output$texto<-renderPrint({
    if(is.null(input$n)){
      return()
    }
    else if(input$n=='Cargados'){
      ncolumna<-input$columna
      if(is.null(input$datoscargados)){
        return()
      }
      else if(input$forma2=='Sesgo'){
        return(skewness(dat()[,ncolumna]))
      }
      else if(input$forma2=='Curtosis'){
        return(sum((dat()[,ncolumna]-mean(dat()[,ncolumna]))^4)/(length(dat()[,ncolumna])*sd(dat()[,ncolumna])^4))
      }
    }
    else if(input$n=='Ejemplos del libro'){
      if(input$forma1=='Sesgo'){
        return(skewness(dat()[,1]))
      }
      else if(input$forma1=='Curtosis'){
        return(sum((dat()[,1]-mean(dat()[,1]))^4)/(length(dat()[,1])*sd(dat()[,1])^4))
      }
    }
    else if(input$n=='Generados aleatoriamente'){
      if(input$forma3=='Sesgo'){
        return(skewness(dat()[,1]))
      }
      else if(input$forma3=='Curtosis'){
        return(sum((dat()[,1]-mean(dat()[,1]))^4)/(length(dat()[,1])*sd(dat()[,1])^4))
      }
    }
  })
  
  
  output$ploteo<-renderPlot({
    if(is.null(input$n)){
      return()
    }
    else if(input$n=='Ejemplos del libro'){
    if(input$forma1=='Sesgo'){
    x<-seq(0,8,0.01)
    Simetrica<-dnorm(x,4,0.5)
    Izquierdo<-df(-x+8,df1=25,df2=26)
    Derecho<-df(x,df1=25,df2=30)
    dat<-data.frame(x,Simetrica,Izquierdo,Derecho)
    
    return(ggplot(dat, aes(x=x))+
      geom_line(aes(y=Simetrica,colour="Simétrica"))+
      geom_area(mapping = aes(x,Simetrica), fill = "blue",alpha = .2)+
      geom_line(aes(y=Izquierdo,colour="Izquierdo"))+
      geom_area(mapping = aes(x,Izquierdo), fill = "green",alpha = .2)+
      geom_line(aes(y=Derecho,colour="Derecho"))+
      geom_area(mapping = aes(x,Derecho), fill = "red",alpha = .2)+
      scale_colour_manual("",values = c("Simétrica"="blue", 
                                        "Izquierdo"="green",
                                        "Derecho"="red")))
    }
    else if(input$forma1=='Curtosis'){
      x<-seq(-5,5,0.01)
      Leptocurtica<-dnorm(x,0,0.5)
      Platicurtica<-dnorm(x,0,2)
      Mesocurtica<-dnorm(x,0,1)
      dat<-data.frame(x,Leptocurtica,Platicurtica,Mesocurtica)
      
      return(ggplot(dat, aes(x=x))+
        geom_line(aes(y=Mesocurtica,colour="Mesocúrtica"))+
        geom_area(mapping = aes(x,Mesocurtica), fill = "red",alpha = .2)+
        geom_line(aes(y=Leptocurtica,colour="Leptocúrtica"))+
        geom_area(mapping = aes(x,Leptocurtica), fill = "blue",alpha = .2)+
        geom_line(aes(y=Platicurtica,colour="Platicúrtica"))+
        geom_area(mapping = aes(x,Platicurtica), fill = "green",alpha = .2)+
        xlim(-5,5)+
        scale_colour_manual("",values = c("Leptocúrtica"="blue", 
                                          "Platicúrtica"="green",
                                          "Mesocúrtica"="red")))
    }
    }
    else if(input$n=='Cargados'){
      if(input$forma2=='Sesgo'){
        x<-seq(0,8,0.01)
        Simetrica<-dnorm(x,4,0.5)
        Izquierdo<-df(-x+8,df1=25,df2=26)
        Derecho<-df(x,df1=25,df2=30)
        dat<-data.frame(x,Simetrica,Izquierdo,Derecho)
        
        return(ggplot(dat, aes(x=x))+
                 geom_line(aes(y=Simetrica,colour="Simétrica"))+
                 geom_area(mapping = aes(x,Simetrica), fill = "blue",alpha = .2)+
                 geom_line(aes(y=Izquierdo,colour="Izquierdo"))+
                 geom_area(mapping = aes(x,Izquierdo), fill = "green",alpha = .2)+
                 geom_line(aes(y=Derecho,colour="Derecho"))+
                 geom_area(mapping = aes(x,Derecho), fill = "red",alpha = .2)+
                 scale_colour_manual("",values = c("Simétrica"="blue", 
                                                   "Izquierdo"="green",
                                                   "Derecho"="red")))
      }
      else if(input$forma2=='Curtosis'){
        x<-seq(-5,5,0.01)
        Leptocurtica<-dnorm(x,0,0.5)
        Platicurtica<-dnorm(x,0,2)
        Mesocurtica<-dnorm(x,0,1)
        dat<-data.frame(x,Leptocurtica,Platicurtica,Mesocurtica)
        
        return(ggplot(dat, aes(x=x))+
                 geom_line(aes(y=Mesocurtica,colour="Mesocúrtica"))+
                 geom_area(mapping = aes(x,Mesocurtica), fill = "red",alpha = .2)+
                 geom_line(aes(y=Leptocurtica,colour="Leptocúrtica"))+
                 geom_area(mapping = aes(x,Leptocurtica), fill = "blue",alpha = .2)+
                 geom_line(aes(y=Platicurtica,colour="Platicúrtica"))+
                 geom_area(mapping = aes(x,Platicurtica), fill = "green",alpha = .2)+
                 xlim(-5,5)+
                 scale_colour_manual("",values = c("Leptocúrtica"="blue", 
                                                   "Platicúrtica"="green",
                                                   "Mesocúrtica"="red")))
      }
    }
    else if(input$n=='Generados aleatoriamente'){
      if(input$forma3=='Sesgo'){
        x<-seq(0,8,0.01)
        Simetrica<-dnorm(x,4,0.5)
        Izquierdo<-df(-x+8,df1=25,df2=26)
        Derecho<-df(x,df1=25,df2=30)
        dat<-data.frame(x,Simetrica,Izquierdo,Derecho)
        
        return(ggplot(dat, aes(x=x))+
                 geom_line(aes(y=Simetrica,colour="Simétrica"))+
                 geom_area(mapping = aes(x,Simetrica), fill = "blue",alpha = .2)+
                 geom_line(aes(y=Izquierdo,colour="Izquierdo"))+
                 geom_area(mapping = aes(x,Izquierdo), fill = "green",alpha = .2)+
                 geom_line(aes(y=Derecho,colour="Derecho"))+
                 geom_area(mapping = aes(x,Derecho), fill = "red",alpha = .2)+
                 scale_colour_manual("",values = c("Simétrica"="blue", 
                                                   "Izquierdo"="green",
                                                   "Derecho"="red")))
      }
      else if(input$forma3=='Curtosis'){
        x<-seq(-5,5,0.01)
        Leptocurtica<-dnorm(x,0,0.5)
        Platicurtica<-dnorm(x,0,2)
        Mesocurtica<-dnorm(x,0,1)
        dat<-data.frame(x,Leptocurtica,Platicurtica,Mesocurtica)
        
        return(ggplot(dat, aes(x=x))+
                 geom_line(aes(y=Mesocurtica,colour="Mesocúrtica"))+
                 geom_area(mapping = aes(x,Mesocurtica), fill = "red",alpha = .2)+
                 geom_line(aes(y=Leptocurtica,colour="Leptocúrtica"))+
                 geom_area(mapping = aes(x,Leptocurtica), fill = "blue",alpha = .2)+
                 geom_line(aes(y=Platicurtica,colour="Platicúrtica"))+
                 geom_area(mapping = aes(x,Platicurtica), fill = "green",alpha = .2)+
                 xlim(-5,5)+
                 scale_colour_manual("",values = c("Leptocúrtica"="blue", 
                                                   "Platicúrtica"="green",
                                                   "Mesocúrtica"="red")))
      }
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)