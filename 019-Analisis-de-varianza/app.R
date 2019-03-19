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

  titlePanel("Análisis de varianza"),
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
                                     label = "Número de variables numéricas",
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
                                     choices= c("Armands","Cars"),
                                     selected = NULL)),

      selectInput( inputId = "factores", label = "Factores ANOVA",
                   choices= c("ANOVA de un factor","ANOVA de dos factores"),
                   selected = NULL),
      conditionalPanel(condition = "input.n=='car' & input.factores=='ANOVA de dos factores'",
                       numericInput(inputId = "vect1", label = "Introducir la columna caracter deseada",
                                min=1,max = 100,step = 1,width = "100%",value = 1)),
      conditionalPanel(condition = "input.n=='gen' & input.factores=='ANOVA de dos factores'",
                       box(title = "Observación", width = NULL, solidHeader = TRUE, status = "warning", "Utilizaremos la columna caracter que hemos incluido a partir de los datos generados para usarla como segundo factor.")),
      conditionalPanel(condition = "input.n=='ejem' & input.factores=='ANOVA de dos factores'",
                       box(title = "Observación", width = NULL, solidHeader = TRUE, status = "warning", "En los datos Armands no hay columnas caracteres para usarlas como segundo factor y en los datos Cars usaremos la primera columna ya que es caracter y además tiene mas de una característica."))

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

      car<-c("Enero","Febrero","Marzo","Abril")
      M1<-rep_len(car,input$m)
      M1<-as.matrix(M1)
      colnames(M1)<-c("Car_1")

      M2<-cbind(M,M1)
      return(M2)

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
      Cars<-read_excel("cars.xlsx")

      if(input$ejemplos=="Armands"){
        Armands
      } else{
        Cars
      }

    }
})

output$table<-renderDataTable({

  if(is.null(input$n)){
    print("Introduzca los datos")
  } else{
     return(data())
  }

},options = list(scrollX=TRUE,scrollY=300,searching=FALSE))

output$table1<-renderPrint ({


  if(input$factores=='ANOVA de un factor'){

    if(is.null(input$n)){
      return("Introduzca los datos")
    }

      else if(input$n=="car"){


         if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
         return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
         }
         else{
         col<-as.numeric(unlist(strsplit(input$vect,",")))
         data1<-as.data.frame(data()[,col])

         g<-lapply(data1,is.character)
         g1<-c()

         for(i in 1:ncol(data1)){
           g1<-c(g1[1],g[[i]])
         }

         if(any(g1)==TRUE){
           print("Inserte sólo columnas numéricas")
         }else{

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

    } else if(input$n=="gen"){
      data1<-as.data.frame(data()[,-(input$f+1)])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w<-summary(aov(g$Valores ~ g$Variables))

      return(w)
    } else if (input$n == "ejem"){

      if(input$ejemplos=="Armands"){

      data1<-as.data.frame(data()[,])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w<-summary(aov(g$Valores ~ g$Variables))

      return(w)
      }else{
        data1<-as.data.frame(data()[,-c(1,2)])

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

  } else if(input$factores=="ANOVA de dos factores"){

    if(is.null(input$n)){
      return("Introduzca los datos")
    }

    else if(input$n=="car"){

      if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
        return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
      }
      else{
        col<-as.numeric(unlist(strsplit(input$vect,",")))
        data1<-as.data.frame(data()[,col])

        #escojer sólo columnas numéricas
      w<-lapply(data1,is.character)
      w1<-c()

      for(i in 1:length(w)){
        w1<-c(w1[1],w[[i]])
      }


      if(any(w1)==TRUE){
        print("Inserte sólo columnas numéricas")
      }else{
        z<-c()

        for(i in 1:ncol(data1)){
          z<-data.frame(Valores=c(z[,1],data1[,i]))
        }

        z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))


        col1<-input$vect1

        data2<-as.data.frame(data()[,col1])

          if(is.character(data2[,1])==TRUE){
          Variables2=rep(data2[,1],times=ncol(data1))
          z2<-data.frame(Variables2)
          g<-cbind(z,z1,z2)

          #ANOVA
          w<-summary(aov(g$Valores ~ g$Variables+g$Variables2))
          w
          } else{
          print("Seleccione una columna caracter")
          }
      }
    }
    } else if(input$n=="gen"){
      data1<-as.data.frame(data()[,-(input$f+1)])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      data2<-as.data.frame(data()[,input$f+1])

      z2<-data.frame(Variables2=rep(data2[,1],times=ncol(data1)))

      g<-cbind(z,z1,z2)

      #ANOVA
      w<-summary(aov(g$Valores ~ g$Variables+g$Variables2))

      return(w)
    } else if(input$n=="ejem"){
      if(input$ejemplos=="Armands"){

        print("No hay columnas caracteres para usarlas como segundo factor")

      }else{
        data1<-as.data.frame(data()[,-c(1,2)])

        z<-c()

        for(i in 1:ncol(data1)){
          z<-data.frame(Valores=c(z[,1],data1[,i]))
        }

        z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

        data2<-as.data.frame(data()[,1])

        z2<-data.frame(Variables2=rep(data2[,1],times=ncol(data1)))

        g<-cbind(z,z1,z2)

        #ANOVA
        w<-summary(aov(g$Valores ~ g$Variables+g$Variables2))

        return(w)
      }
  }
}

})


output$box<-renderPlot ({

if(input$factores=='ANOVA de un factor'){

    if(is.null(input$n)){
      return("Introduzca los datos")
    }

    else if(input$n=="car"){


      if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
        return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
      }
      else{
        col<-as.numeric(unlist(strsplit(input$vect,",")))
        data1<-as.data.frame(data()[,col])

        g<-lapply(data1,is.character)
        g1<-c()

        for(i in 1:ncol(data1)){
          g1<-c(g1[1],g[[i]])
        }

        if(any(g1)==TRUE){
          print("Inserte sólo columnas numéricas")
        }else{

        z<-c()

        for(i in 1:ncol(data1)){
          z<-data.frame(Valores=c(z[,1],data1[,i]))
        }

        z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

        g<-cbind(z,z1)

        #ANOVA
        w1<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))+
          labs( title = "Gráfico de caja para las columnas numéricas",
                x = " ", y = " ",caption = "https://synergy.vision/" )

        return(w1)
        }
      }

    } else if(input$n=="gen"){
      data1<-as.data.frame(data()[,-(input$f+1)])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w2<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))+
        labs( title = "Gráfico de caja para las columnas numéricas",
              x = " ", y = " ",caption = "https://synergy.vision/" )

      return(w2)

    } else if(input$n=="ejem"){

      if(input$ejemplos=="Armands"){

      data1<-as.data.frame(data())

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w2<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))+
        labs( title = "Gráfico de caja para las columnas numéricas",
              x = " ", y = " ",caption = "https://synergy.vision/" )

      return(w2)
      } else{
      data1<-as.data.frame(data()[,-c(1,2)])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      g<-cbind(z,z1)

      #ANOVA
      w2<-ggplot(data = g, aes(x=Variables, y=Valores)) + geom_boxplot(aes(fill=Variables))+
        labs( title = "Gráfico de caja para las columnas numéricas",
              x = " ", y = " ",caption = "https://synergy.vision/" )

      return(w2)
      }
    }
} else if(input$factores=="ANOVA de dos factores"){

  if(is.null(input$n)){
    return("Introduzca los datos")
  }

  else if(input$n=="car"){

    if(is.null(input$datoscargados) | isTRUE(input$vect=="") ){
      return('Introduzca los datos y escoja las columnas numéricas deseadas (mínimo dos columnas).')
    }
    else{
      col<-as.numeric(unlist(strsplit(input$vect,",")))
      data1<-as.data.frame(data()[,col])

      #escojer sólo columnas numéricas
      w<-lapply(data1,is.character)
      w1<-c()

      for(i in 1:length(w)){
        w1<-c(w1[1],w[[i]])
      }


      if(any(w1)==TRUE){
        print("Inserte sólo columnas numéricas")
      }else{
        z<-c()

        for(i in 1:ncol(data1)){
          z<-data.frame(Valores=c(z[,1],data1[,i]))
        }


        col1<-input$vect1

        data2<-as.data.frame(data()[,col1])

        if(is.character(data2[,1])==TRUE){
          Variables2=rep(data2[,1],times=ncol(data1))
          z2<-data.frame(Variables2)
          g<-cbind(z,z2)

          w2<-ggplot(data = g, aes(x=Variables2, y=Valores)) + geom_boxplot(aes(fill=Variables2))+
            labs( title = "Gráfico de caja parala columna caracter",
                  x = " ", y = " ",caption = "https://synergy.vision/" )
          return(w2)

        } else{
          print("Seleccione una columna caracter")
        }
      }
    }
  } else if(input$n=="gen"){
    data1<-as.data.frame(data()[,-(input$f+1)])

    z<-c()

    for(i in 1:ncol(data1)){
      z<-data.frame(Valores=c(z[,1],data1[,i]))
    }

    data2<-as.data.frame(data()[,input$f+1])
    z1<-data.frame(Variables2=rep(data2[,1],times=ncol(data1)))

    g<-cbind(z,z1)

    #ANOVA
    w2<-ggplot(data = g, aes(x=Variables2, y=Valores)) + geom_boxplot(aes(fill=Variables2))+
      labs( title = "Gráfico de caja para la columna caracter",
            x = " ", y = " ",caption = "https://synergy.vision/" )

    return(w2)
  } else if(input$n=="ejem"){
    if(input$ejemplos=="Armands"){

      print("No hay columnas caracteres para usarlas como segundo factor")

    }else{
      data1<-as.data.frame(data()[,-c(1,2)])

      z<-c()

      for(i in 1:ncol(data1)){
        z<-data.frame(Valores=c(z[,1],data1[,i]))
      }

      z1<-data.frame(Variables=rep(c(colnames(data1)[1:ncol(data1)]),each=nrow(data1)))

      data2<-as.data.frame(data()[,1])

      z2<-data.frame(Variables2=rep(data2[,1],times=ncol(data1)))

      g<-cbind(z,z1,z2)

      w2<-ggplot(data = g, aes(x=Variables2, y=Valores)) + geom_boxplot(aes(fill=Variables2))+
        labs( title = "Gráfico de caja para la columna categórica",
              x = " ", y = " ",caption = "https://synergy.vision/" )

      return(w2)
    }
  }
}

})






  }


shinyApp(ui = ui, server = server)
