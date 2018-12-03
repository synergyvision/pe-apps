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
ensure_version("RAM","1.2.1.7")
ensure_version("rje","1.9")
ensure_version("futile.logger","1.4.3")


library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(modeest)
library(matrixStats)
library(ggplot2)
library(sets)
library(RAM)
library(rje)
library(futile.logger)


ui <- fluidPage(
  
  titlePanel("Teoría de Conjuntos"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      textInput(inputId = "con1", label = "Introducir el primer conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con2", label = "Introducir el segundo conjunto",placeholder = "a,b,..."),
      
      
      conditionalPanel(condition="input.con1",selectInput(inputId = "ope",label = "Operaciones de cojuntos",choices = c("Unión","Intersección","Diferencia","Complemento","Producto Cartesiano","Potencia"),
                                                         selected = NULL),checkboxGroupInput(inputId = "con",label="Selección de Conjuntos",
                                                                   choices = c("Conjunto 1"="con1","Conjunto 2"="con2",selected=NULL)))
      
      ),

    mainPanel(
      
      column(width=5,h3("Operación"),div(style="height:400px; overflow-y: scroll",verbatimTextOutput("op1"))),
      
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
 
      
    d<-reactive({
     w<-rbind(conjunto1(),conjunto2())
      row.names(w)<-c("con1","con2")
      return(w)
      
    })


 output$op1<-renderPrint({
   
    if(is.null(input$con)){
      return(NULL)
    } else{
      if(input$ope=="Unión"){
        if(length(input$con)==1){
          union(d()[input$con[1],],d()[input$con[1],])
        } else{
      union(d()[input$con[1],],d()[input$con[2],])
        }
      } else if(input$ope=="Intersección"){
        if(length(input$con)==1){
          intersect(d()[input$con[1],],d()[input$con[1],])
        } else{
          intersect(d()[input$con[1],],d()[input$con[2],])
        }
      }
        else if(input$ope=="Diferencia"){
          if(length(input$con)==1){
            setdiff(d()[input$con[1],],d()[input$con[1],])
          } else{
            #Diferencia Conjunto 1 - Conjunto 2
           setdiff(d()[input$con[1],],d()[input$con[2],])
          }}
        else if(input$ope=="Producto Cartesiano"){
            
            cartesian <- function(a, b) {
              axb <- list()
              k <- 1
              for (i in a) {
                for (j in b) {
                  axb[[k]] <- c(i,j)
                  k <- k + 1
                }
              }
              return(axb)
            }
            
            if(length(input$con)==1){
              cartesian(d()[input$con[1],],d()[input$con[1],])
            } else{
              cartesian(d()[input$con[1],],d()[input$con[2],])
              }
        } else if(input$ope=="Potencia"){
          if(length(input$con)==1){
            powerSet(d()[input$con[1],])
          } else{
            powerSet(d()[input$con[2],])
          }
        }
       
     }
   
})
 
output$plot<-renderPlot({
  if(is.null(input$con)){
    return(NULL)
  }
  else if(input$ope=="Unión"){
    a<-d()[input$con[1],]
    b<-d()[input$con[2],]
  
    group.venn(list(Conjunto1=a, Conjunto2=b), label=TRUE, 
               fill = c("blue", "blue"),
               cat.pos = c(0, 0),
               lab.cex=1.1,file = NULL,ext = NULL)
    
  } else if(input$ope=="Intersección"){
    a1<-d()[input$con[1],]
    b1<-d()[input$con[2],]
    
    group.venn(list(Conjunto1=a1, Conjunto2=b1), label=TRUE, 
               fill = c("green", "blue"),
               cat.pos = c(0, 0),
               lab.cex=1.1,file = NULL,ext = NULL)
    
  } else if(input$ope=="Diferencia"){
    a1<-d()[input$con[1],]
    b1<-d()[input$con[2],]
    
    group.venn(list(Conjunto1=a1, Conjunto2=b1), label=TRUE, 
               fill = c("green", "white"),
               cat.pos = c(0, 0),
               lab.cex=1.1,file = NULL,ext = NULL)
  } 
})

}

shinyApp(ui = ui, server = server)
