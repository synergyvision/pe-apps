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

  titlePanel("Analisis de varianza"),
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
                                  buttonLabel = "Buscar...", placeholder = "Aun no seleccionas el archivo..."),
                        textInput(inputId = "vect", label = "Introducir las columnas numéricas deseadas",
                                  placeholder = "1,2,...")


      ),

      conditionalPanel( condition = "input.n == 'ejem'",
                        selectInput( inputId = "ejemplos", label = "Datos de ejemplo",
                                     choices= c("Armands"),
                                     selected = NULL)),

      selectInput( inputId = "factores", label = "Factores ANOVA",
                   choices= c("ANOVA de un factor","ANOVA de dos factores"),
                   selected = NULL)


    ),
    mainPanel(
    tabsetPanel(type = 'tabs',id='anova',
     tabPanel("Datos",br(),dataTableOutput("table")),
     tabPanel('Gráfico de caja',br(),plotOutput('box')),
     tabPanel("ANOVA",br(),br(),'Tabla ANOVA',verbatimTextOutput("table1")))

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

      Armands<-read_excel("Armand's.xlsx")

    }
})

output$table<-renderDataTable({

  if(is.null(input$n)){
    return(print("Introduzca los datos"))
  } else{
     return(data())
  }

},options = list(scrollX=TRUE,scrollY=300,searching=FALSE))

# s<-data.frame(Precio=c(a[,1],a[,2],a[,3]))
# variables<-rep(c(colnames(a)[1:3]),each=10)


output$table1<-renderPrint ({

  if(is.null(input$n)){
    return("Introduzca los datos")
  }

  else if(input$factores=='ANOVA de un factor'){

      if(input$n=="car"){


         if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
         return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
         }
         else{
         col<-as.numeric(unlist(strsplit(input$vect,",")))
         data1<-as.data.frame(data()[,col])

         z<-c()

         for(i in 1:ncol(data1)){
           z<-data.frame(Valores=c(z[,1],data1[,i]))
         }

         z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

         g<-cbind(z,z1)

         #ANOVA
         w<-summary(aov(g$Valores ~ g$Variables))

         return(w)
         }

    } else{
      data1<-as.data.frame(data())

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w<-summary(aov(g$Valores ~ g$Variables))

      return(w)
   }
}

})


output$box<-renderPlot ({

  if(is.null(input$n)){
    return("Introduzca los datos")
  }

  else if(input$factores=='ANOVA de un factor'){

    if(input$n=="car"){


      if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
        return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
      }
      else{
        col<-as.numeric(unlist(strsplit(input$vect,",")))
        data1<-as.data.frame(data()[,col])

        z<-c()

        for(i in 1:ncol(data1)){
          z<-data.frame(Valores=c(z[,1],data1[,i]))
        }

        z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

        g<-cbind(z,z1)

        #ANOVA
        w1<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))

        return(w1)
      }

    } else{
      data1<-as.data.frame(data())

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w2<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))

      return(w2)
    }
  }

})






  }


shinyApp(ui = ui, server = server)
