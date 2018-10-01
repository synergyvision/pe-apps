library(shiny)
library('ggplot2')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Distribución de Frecuencia"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      

      selectInput( inputId = "n", 
                   label = "Tipo de frecuencia",
                   choices= c('Frecuencia Acumulada','Frecuencia Acumulada Relativa'), 
                   selected = NULL)

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      tableOutput('tabla'),
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  sueldos <- c(47,47,47,47,48,49,50,50,50,51,51,51,51,52,52,52,52,52,52,54,54,
               54,54,54,57,60,49,49,50,50,51,51,51,51,52,52,56,56,57,57,52,52)
  
  output$tabla<-renderTable({
    
    if(input$n=='Frecuencia Acumulada'){
      
      fr<-data.frame(table(sueldos))
      fa<-transform(fr,fAcum=cumsum(Freq))
      colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada")
      return(fa)
    }
    
    else if(input$n=='Frecuencia Acumulada Relativa'){
      
      fr<-data.frame(table(sueldos))
      fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
      colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada Relativa")
      return(fa)
    }
    
  },digits=4)
  
  output$distPlot <- renderPlot({
    
    if(input$n=='Frecuencia Acumulada'){
      
      fr<-data.frame(table(sueldos))
      fa<-transform(fr,fAcum=cumsum(Freq))
      colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada")
    
      dat<-data.frame(sueldos=as.numeric(fa$Sueldos),
                      facum=as.numeric(fa$`Frecuencia Acumulada`))
      
      ggplot(dat,mapping = aes(sueldos,facum))+ 
        geom_point(colour="blue" )+
        geom_line( colour="blue")+
        labs(title = "Distribución de Frecuencia", x="x", 
             y="Frecuencia Acumulada")
    
      
    }
    
    else if(input$n=='Frecuencia Acumulada Relativa'){
      
      fr<-data.frame(table(sueldos))
      fa<-transform(fr,FreAcuRel=cumsum(prop.table(`Freq`)))
      colnames(fa)<-c("Sueldos","Frecuencia","Frecuencia Acumulada Relativa")
      
      dat<-data.frame(sueldos=as.numeric(fa$Sueldos),
                      far=as.numeric(fa$`Frecuencia Acumulada Relativa`))
      
      ggplot(dat,mapping = aes(sueldos,far))+ 
        geom_point(colour="blue" )+
        geom_line( colour="blue")+
        labs(title = "Distribución de Frecuencia", x="x", 
             y="Frecuencia Acumulada")
      
    }


    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
