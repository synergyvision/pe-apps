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

library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(modeest)
library(matrixStats)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Teoría de Conjuntos"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      textInput(inputId = "con1", label = "Introducir el primer conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con2", label = "Introducir el segundo conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con3", label = "Introducir el tercer conjunto",placeholder = "a,b,..."),
      
      conditionalPanel(condition = "input.con3", checkboxGroupInput(inputId = "con",label="Selección de Conjuntos",
                                                                    choices = c("Conjunto 1","Conjunto 2","Conjunto 3",selected=NULL))),
      conditionalPanel(condition="input.con",selectInput(inputId = "ope",label = "Operaciones de cojuntos",choices = c("Unión","Intersección","Diferencia","Complemento","Producto Cartesiano","Potencia"),
                                                         selected = NULL))
    ),
    mainPanel(
      
      column(width=5,h3("Datos"),div(style="height:400px; overflow-y: scroll",tableOutput("table"))),
      
      conditionalPanel(condition ="input.medias=='Cuantiles'",column(width=7,h3("Cuantiles"),verbatimTextOutput("cuantiles"))),
      conditionalPanel(condition ="input.medias=='Diagrama de Caja'",column(width=7,h3("Diagrama de Caja"),plotOutput("plot")))
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
    rownames(M)<-c(paste0("fil_",1:input$m))
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
 
 
 output$table <- renderTable({ data() },
                              striped = TRUE,hover = TRUE,
                              bordered = TRUE,rownames = FALSE)
 
 
 
 output$cuantiles<-renderPrint({
   
   if(is.null(input$medias)||is.null(data())){
     return()
   }
  
   if(input$medias=="Cuantiles"){
      if(is.null(input$vect)){
        return()
       } else{
       x <- as.numeric(unlist(strsplit(input$vect,",")))
    
       apply(data(),2,quantile,probs = x)
       
       }
     
   }
})
   
   output$plot<-renderPlot({
     
     if(input$medias=="Diagrama de Caja"){
       
       if(input$n=="gen"){
         datos<-as.data.frame(data())
         
         ncol<-input$k
         
         ggplot(datos, aes(x=length(datos[,ncol]),y=datos[,ncol]))+
           geom_boxplot(fill="blue",alpha=0.5)+
           labs(title = "Diagrama de Caja",
                x=colnames(datos[ncol]),y="")
       }
       
       else if(input$n=="car"){
       datos<-as.data.frame(data())
       
       ncol<-input$d
       
       ggplot(datos, aes(x=length(datos[,ncol]),y=datos[,ncol]))+
         geom_boxplot(fill="blue",alpha=0.5)+
         labs(title = "Diagrama de Caja",
              x=colnames(datos[ncol]),y="")
       }
       else if(input$n=="ejem"){
         datos<-as.data.frame(data())

         ggplot(datos, aes(x=length(datos[,1]),y=datos[,1]))+
           geom_boxplot(fill="blue",alpha=0.5)+
           labs(title = "Diagrama de Caja",
                x=colnames(datos[1]),y="")
       }
       
     }  
     
     
   })
                             
 
}


shinyApp(ui = ui, server = server)
