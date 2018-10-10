library(shiny)
library('ggplot2')
library('readxl')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Distribución de Frecuencia"),

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
                                                     min = 2,
                                                     max = 20,
                                                     value = 2)
                        ),
                        selectInput( inputId = "n1", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia Acumulada','Frecuencia Acumulada Relativa'), 
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
                                                     min = 2,
                                                     max = 20,
                                                     value = 2)
                        ),
                        selectInput( inputId = "n2", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia Acumulada','Frecuencia Acumulada Relativa'), 
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
                                                     min = 2,
                                                     max = 20,
                                                     value = 2)
                        ),
                        selectInput( inputId = "n3", 
                                     label = "Tipo de frecuencia:",
                                     choices= c('Frecuencia Acumulada','Frecuencia Acumulada Relativa'), 
                                     selected = NULL)
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      column(width = 3,tableOutput('tabla')),
      column(width = 9,fluidRow(tableOutput('tabla1')),fluidRow(plotOutput('distPlot')))
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
        as.data.frame(read_excel(infile2$datapath))
      }
    }
    
    else if(infile=='Generados aleatoriamente'){
      data.frame(Datos=sample(80:100,input$CantidadDatos,replace = TRUE))
    }
    
  })
  
  output$tabla1<-renderTable({
    
    if(is.null(input$n)){
      return()
    }
    
    else if(input$n=='Ejemplos del libro'){
    
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
        
        clase<-cut(dat()[,1],breaks = intervalo,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
      }
    
      else if(input$interval=='Manual'){
        
        intervalo<-input$bins1
        clase<-cut(dat()[,1],breaks = intervalo,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
    
      }
      
    }
    
    else if(input$n=='Generados aleatoriamente'){
      
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
        
        clase<-cut(dat()[,1],breaks = intervalo1,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
        
      }
      
      else if(input$interval2=='Manual'){
        
        intervalo1<-input$bins3
        clase<-cut(dat()[,1],breaks = intervalo1,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
        
      }
      
    }
    
    else if(input$n=='Cargados'){
      
      ncolumna<-input$columna
      
      if(is.null(input$interval1)){
        return()
      }
      
      else if(input$interval1=='Métodos dados'){
        
        intervalo2<-if(input$metodo2=='Fórmula de Sturges'){
          nclass.Sturges(dat()[,ncolumna])
        }
        else if(input$metodo2=='Regla de Scott'){
          nclass.scott(dat()[,ncolumna])
        }
        else if(input$metodo2=='Selección de Freedman-Diaconis'){
          nclass.FD(dat()[,ncolumna])
        }
        
        clase<-cut(dat()[,ncolumna],breaks = intervalo2,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
        
      }
      
      else if(input$interval1=='Manual'){
        
        intervalo2<-input$bins2
        
        clase<-cut(dat()[,ncolumna],breaks = intervalo2,include.lowest = TRUE,right = FALSE)
        
        if(input$n1=='Frecuencia Acumulada'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,fAcum=cumsum(Freq))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada")
          return(fa)
        }
        
        else if(input$n1=='Frecuencia Acumulada Relativa'){
          
          fr<-data.frame(table(clase))
          fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
          colnames(fa)<-c("Intervalos","Frecuencia","Frecuencia Acumulada Relativa")
          return(fa)
        }
        
      }
      
    }
    },digits = 4)
  
  output$tabla<-renderTable({
    return(dat())
    })
  
  #output$distPlot <- renderPlot({
  #   
  #   if(input$n=='Frecuencia Acumulada'){
  #     
  #     fr<-data.frame(table(sueldos))
  #     fa<-transform(fr,fAcum=cumsum(Freq))
  #     colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada")
  #   
  #     dat<-data.frame(sueldos=as.numeric(fa$Sueldos),
  #                     facum=as.numeric(fa$`Frecuencia Acumulada`))
  #     
  #     ggplot(dat,mapping = aes(sueldos,facum))+ 
  #       geom_point(colour="blue" )+
  #       geom_line( colour="blue")+
  #       labs(title = "Distribución de Frecuencia", x="x", 
  #            y="Frecuencia Acumulada")
  #   
  #     
  #   }
  #   
  #   else if(input$n=='Frecuencia Acumulada Relativa'){
  #     
  #     fr<-data.frame(table(sueldos))
  #     fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
  #     colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada Relativa")
  #     
  #     dat<-data.frame(sueldos=as.numeric(fa$Sueldos),
  #                     far=as.numeric(fa$`Frecuencia Acumulada Relativa`))
  #     
  #     ggplot(dat,mapping = aes(sueldos,far))+ 
  #       geom_point(colour="blue" )+
  #       geom_line( colour="blue")+
  #       labs(title = "Distribución de Frecuencia", x="x", 
  #            y="Frecuencia Acumulada")
  #     
  #   }
  # 
  # 
  #   })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



# selectInput( inputId = "n", 
#              label = "Tipo de frecuencia",
#              choices= c('Frecuencia Acumulada','Frecuencia Acumulada Relativa'), 
#              selected = NULL)