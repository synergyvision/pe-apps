ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydasboard", "0.7.0")
ensure_version("psych", "1.8.4")
ensure_version("modeest", "2.1")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.0.0")
ensure_version("sets","1.0-18")

library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(modeest)
library(matrixStats)
library(ggplot2)
library(sets)

ui <- fluidPage(
  
  titlePanel("Teoría de Conjuntos"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      textInput(inputId = "con1", label = "Introducir el primer conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con2", label = "Introducir el segundo conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con3", label = "Introducir el tercer conjunto",placeholder = "a,b,..."),
      
      
      conditionalPanel(condition="input.con1",selectInput(inputId = "ope",label = "Operaciones de cojuntos",choices = c("Unión","Intersección","Diferencia","Complemento","Producto Cartesiano","Potencia"),
                                                         selected = NULL),checkboxGroupInput(inputId = "con",label="Selección de Conjuntos",
                                                                   choices = c("Conjunto 1"="con1","Conjunto 2"="con2","Conjunto 3"="con3",selected=NULL)))
      
      ),

    mainPanel(
      
      column(width=5,h3("Operación"),verbatimTextOutput("op1")),
      
      column(width=5,h3("Diagrama de Venn"),plotOutput("plot"))
    )
  )
)



server <- function(input, output,session) {
  
  conjunto1<-reactive({
    if(is.null(input$con=="con1")){
      return(NULL)
    } else{
      as.vector(unlist(strsplit(input$con1,",")))
    }
  })
  
   conjunto2<-reactive({
    if(is.null(input$con=="con2")){
      return(NULL)
    } else{
      as.vector(unlist(strsplit(input$con2,",")))
    }
})

    conjunto3<-reactive({
      if(is.null(input$con=="con3")){
       return(NULL)
      } else{
        as.vector(unlist(strsplit(input$con3,",")))
      }
     })

    # observe({
    #   vars<-names(data())
    #   updateCheckboxGroupInput(session, 'columnas', choices = vars)
    # })  
    
    
    
d<-reactive({
  w<-rbind(conjunto1(),conjunto2(),conjunto3())
  # rownames(w)[1:length(c(input$con))]<-c(input$con)
  
  return(w[1:length(c(input$con)),])
  
})


 output$op1<-renderPrint({
   
   return(d())
})

}

shinyApp(ui = ui, server = server)
