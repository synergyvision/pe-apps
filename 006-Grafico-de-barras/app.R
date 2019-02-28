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
  titlePanel("Gráficos de barras"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(


      radioButtons(inputId="n",
                   label = "Origen de los datos",
                   choices = c('Generados','Cargados','Ejemplos'),
                   selected = " "),
      conditionalPanel( condition = "input.n=='Ejemplos'",
                        selectInput( inputId = "m",
                                     label = "Datos de ejemplos",
                                     choices= c('Sueldos','Otros','Tiempo de uso de equipos'),
                                     selected = NULL)
                        ),
      conditionalPanel( condition = "input.n=='Cargados'",
                        fileInput( inputId = "datoscargados",
                                   label = "Seleccionar desde un archivo guardado", buttonLabel = "Buscar...",
                                   placeholder = "Aun no seleccionas el archivo..."),
                        numericInput( inputId = "columna",
                                      label="Escoja el número de columna deseado",
                                      min = 1,
                                      max = 100,
                                      step = 1,
                                      value = 1,
                                      width = "100%")
                        ),
      conditionalPanel( condition = "input.n=='Generados'",
                        sliderInput(inputId = "CantidadDatos",
                                    label = "Cantidad de datos",
                                    min = 1,
                                    max = 100,
                                    value = 5)
                      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type='tabs',id='f',
                  tabPanel('Datos',br(),dataTableOutput(outputId = "tabla")),
                  tabPanel('Gráfico de barra',br(),plotOutput(outputId = "distPlot"))
                  )

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

    else if(infile=='Ejemplos'){

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

    else if(infile=='Generados'){
      data.frame(Datos=sample(80:100,input$CantidadDatos,replace = TRUE))
    }

  })

vars<-reactive({
   if(input$n=='Cargados'){
      names(dat())[input$columna]
    }
    else{
      names(dat())
    }
  })

fr<-reactive({
  if(input$n=='Cargados'){
        if(is.null(input$datoscargados)){
          return()
        }
        else{
        h<-data.frame(table(dat()[,input$columna]))
        colnames(h)<-c(vars(),"Frecuencia")
        return(h)
        }
  }
  else{
    h<-data.frame(table(dat()))
    colnames(h)<-c(vars(),"Frecuencia")
    return(h)
  }
})

output$tabla<-renderDataTable({
  if(is.null(input$n)){
        return()
  }
  return(fr())
},options = list(scrollX=TRUE,scrollY=300,searching=FALSE))


  output$distPlot<-renderPlot({
    if(is.null(input$n)){
      return()
    }
    else if(input$n=='Cargados'){
      if(is.null(input$datoscargados)){
      return()
      }
      else{
        ggplot(fr(), aes(x=fr()[,1],y=Frecuencia))+
          geom_bar(stat = "identity", color="black",
                   fill="Blue", alpha=0.5)+
          labs(title = "Diagrama de barra", x=vars(),y="Frecuencia",caption = "https://synergy.vision/")
      }
    }
    else{
      ggplot(fr(), aes(x=fr()[,1],y=Frecuencia))+
        geom_bar(stat = "identity", color="black",
                 fill="Blue", alpha=0.5)+
        labs(title = "Diagrama de barra", x=vars(),y="Frecuencia",caption = "https://synergy.vision/")
    }
  })

  # output$tabla<-renderTable({
  #   if(is.null(input$n)){
  #     return()
  #   }
  #   else if(input$n=='Cargados'){
  #     if(is.null(input$datoscargados)){
  #       return()
  #     }
  #     else{
  #     fr<-data.frame(table(dat()[,input$columna]))
  #     colnames(fr)<-c(vars(),"Frecuencia")
  #     return(fr)
  #     }
  #   }
  #   else{
  #     fr<-data.frame(table(dat()))
  #     colnames(fr)<-c(vars(),"Frecuencia")
  #     return(fr)
  #   }
  # },digits = 1)

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
