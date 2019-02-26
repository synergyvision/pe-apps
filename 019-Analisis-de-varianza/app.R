ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.2.0")
ensure_version("readxl", "1.2.0")
ensure_version("shinydashboard", "0.7.1")
ensure_version("psych", "1.8.10")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.1.0")




library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
library(matrixStats)
library(ggplot2)


ui <- fluidPage(

  titlePanel("Analisis de Varianza"),
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

      selectInput( inputId = "factores", label = "Factores ANOVA",
                   choices= c("ANOVA de un factor","ANOVA de dos factores"),
                   selected = NULL)


    ),
    mainPanel(
    tabsetPanel(type = 'tabs',id='anova',
     tabPanel("Datos",br(),dataTableOutput("table")),
     tabPanel("ANOVA",br(),br(),verbatimTextOutput("tabla1")))

    )
  )

)



server <- function(input, output,session) {

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

output$table<-renderDataTable({
data()
},options = list(scrollX=TRUE,scrollY=300,searching=FALSE))

# s<-data.frame(Precio=c(a[,1],a[,2],a[,3]))
# variables<-rep(c(colnames(a)[1:3]),each=10)

  }


shinyApp(ui = ui, server = server)
