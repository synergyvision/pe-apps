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

  titlePanel("Pruebas de Hipotesis"),
  sidebarLayout(

    sidebarPanel(width = 3,

  selectInput(inputId = 'ph',label = 'Escoja la Prueba de Hipótesis deseada',choices = c('Media de una población','Diferencia de medias de dos poblaciones',
                                                                                         'Varianza de una población','Igualdad de varianzas de dos poblaciones','Proporción en una población'),
              selected = NULL),
  conditionalPanel(condition = "input.ph == 'Media de una población'", selectInput(inputId = 'vc',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza conocida'", selectInput(inputId = 'tp',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza desconocida'", selectInput(inputId = 'tp1',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones'", selectInput(inputId = 'vc1',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza conocida'", selectInput(inputId = 'tp11',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
                  conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza desconocida'", selectInput(inputId = 'tp12',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Varianza de una población'", selectInput(inputId = 'tp2',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Igualdad de varianzas de dos poblaciones'", selectInput(inputId = 'tp3',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL)),
  conditionalPanel(condition = "input.ph == 'Proporción en una población'", selectInput(inputId = 'tp4',label = 'Tipo de Prueba',choices = c('Dos colas','Cola superior','Cola inferior'),selected = NULL))
    ),

  mainPanel(withMathJax(),width = 9,
    conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza conocida'",column(width=3,numericInput(inputId = 'MediaHip',label = HTML('Inserte Media hipotética &mu;<sub>o</sub>'),min=0,max = 100,value = 0,step = 0.1,width = '150px'),
                     numericInput(inputId = 'MediaMuestral',label = 'Inserte Media de la muestra',min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                     numericInput(inputId = 'Muestra',label = 'Inserte Tamaño de la muestra',min=0,max = 100,value = 5,step = 1,width = '150px'),
                     numericInput(inputId = 'VarianzaPob',label = 'Inserte Varianza Poblacional',min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
                     numericInput(inputId = 'signif',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),column(width = 8,align='center',plotOutput('grafica1'))
                     ),
    conditionalPanel(condition = "input.ph == 'Media de una población' & input.vc == 'Varianza desconocida'",column(width=3,numericInput(inputId = 'MediaHip1',label = HTML('Inserte Media hipotética &mu;<sub>o</sub>'),min=0,max = 100,value = 0,step = 0.1,width = '150px'),
                                                                                                                 numericInput(inputId = 'MediaMuestral1',label = 'Inserte Media de la muestra',min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                                                                                                                 numericInput(inputId = 'Muestra1',label = 'Inserte Tamaño de la muestra',min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                 numericInput(inputId = 'Varianzamu1',label = 'Inserte Varianza muestral',min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
                                                                                                                numericInput(inputId = 'signif1',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),column(width = 8,align='center',plotOutput('grafica2'))),

    conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza conocida'",column(width = 2,numericInput(inputId = 'Media1Hip',label = HTML('Inserte Media &mu;<sub>x</sub>'),min=0,max = 100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media2Hip',label = HTML('Inserte Media &mu;<sub>y</sub>'),min=0,max =100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media1Muestral',label = HTML('Inserte Media de la muestra X&#772;'),min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media2Muestral',label = HTML('Inserte Media de la muestra Y&#772;'),min=0,max = 100,value = 7,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'signif2',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
                                                                                                                                   ),
                                                                                                                            column(width = 2,br(),numericInput(inputId = 'Muestra11',label = HTML('Inserte Tamaño de la muestra n<sub>x</sub>'),min=0,max = 100,value = 10,step = 1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Muestra12',label = HTML('Inserte Tamaño de la muestra n<sub>y</sub>'),min=0,max = 100,value = 15,step = 1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Varianza1Pob',label = HTML('Inserte Varianza Poblacional &sigma;<sup>2</sup><sub>x</sub>'),min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Varianza2Pob',label = HTML('Inserte Varianza Poblacional &sigma;<sup>2</sup><sub>y</sub>'),min=0.1,max = 50,value = 2,step = 0.1,width = '150px')
                                                                                                                                   ),
                                                                                                                            column(width = 8,align='center',plotOutput('grafica3'))),
    conditionalPanel(condition = "input.ph == 'Diferencia de medias de dos poblaciones' & input.vc1 == 'Varianza desconocida'",column(width = 2,numericInput(inputId = 'Media1Hip1',label = HTML('Inserte Media &mu;<sub>x</sub>'),min=0,max = 100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media2Hip1',label = HTML('Inserte Media &mu;<sub>y</sub>'),min=0,max =100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media1Muestral1',label = HTML('Inserte Media de la muestra X&#772;'),min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'Media2Muestral1',label = HTML('Inserte Media de la muestra Y&#772;'),min=0,max = 100,value = 7,step = 0.1,width = '150px'),
                                                                                                                                   numericInput(inputId = 'signif3',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),
    column(width = 2,br(),numericInput(inputId = 'Muestra13',label = HTML('Inserte Tamaño de la muestra n<sub>x</sub>'),min=0,max = 100,value = 10,step = 1,width = '150px'),
           numericInput(inputId = 'Muestra14',label = HTML('Inserte Tamaño de la muestra n<sub>y</sub>'),min=0,max = 100,value = 15,step = 1,width = '150px'),
           numericInput(inputId = 'Varianza1Mu1',label = HTML('Inserte Varianza Muestral S<sup>2</sup><sub>x</sub>'),min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
           numericInput(inputId = 'Varianza2Mu2',label = HTML('Inserte Varianza Muestral S<sup>2</sup><sub>y</sub>'),min=0.1,max = 50,value = 2,step = 0.1,width = '150px')
    ),
    column(width = 8,align='center',plotOutput('grafica4')))
    ,


     conditionalPanel(condition = "input.ph == 'Varianza de una población'",column(width=3,numericInput(inputId = 'SigmaHip',label = HTML('Inserte Varianza hipotética &sigma;<sup>2</sup><sub>o</sub>'),min=0.1,max = 50,value = 1,step = 0.1,width = '150px'),
                                                                                           numericInput(inputId = 'SigmaMuestral',label = HTML('Inserte Varianza Muestral S<sup>2</sup>'),min=0.1,max = 50,value = 2,step = 0.1,width = '150px'),
                                                                                           numericInput(inputId = 'MuestraVar',label = 'Inserte Tamaño de la muestra',min=0,max = 100,value = 15,step = 1,width = '150px'),
                                                                                           numericInput(inputId = 'signif4',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px'))
     ,column(width = 8,align='center',plotOutput('grafica5'))),


    conditionalPanel(condition = "input.ph == 'Igualdad de varianzas de dos poblaciones'",column(width = 2,numericInput(inputId = 'var1Hip1',label = HTML('Inserte Varianza &sigma;<sup>2</sup><sub>x</sub>'),min=0,max = 100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                      numericInput(inputId = 'var2Hip1',label = HTML('Inserte Varianza &sigma;<sup>2</sup><sub>y</sub>'),min=0,max =100,value = 1,step = 0.1,width = '150px'),
                                                                                                                                      numericInput(inputId = 'var1Muestral1',label = HTML('Inserte Varianza de la muestra S<sup>2</sup><sub>x</sub>'),min=0,max = 100,value = 5,step = 0.1,width = '150px'),
                                                                                                                                      numericInput(inputId = 'var2Muestral1',label = HTML('Inserte Varianza de la muestra S<sup>2</sup><sub>y</sub>'),min=0,max = 100,value = 7,step = 0.1,width = '150px'),
                                                                                                                                      numericInput(inputId = 'signif5',label = 'Inserte Nivel de Significancia',min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')
    ),
    column(width = 2,br(),numericInput(inputId = 'Muestra4',label = HTML('Inserte Tamaño de la muestra n<sub>x</sub>'),min=0,max = 100,value = 10,step = 1,width = '150px'),
           numericInput(inputId = 'Muestra5',label = HTML('Inserte Tamaño de la muestra n<sub>y</sub>'),min=0,max = 100,value = 15,step = 1,width = '150px')
    ),
    column(width = 8,align='center',plotOutput('grafica6'))),

    conditionalPanel(condition = "input.ph == 'Proporción en una población'",column(width = 2,numericInput(inputId = 'PropHip',label = HTML('Inserte Proporción hipotética <i>p</i><sub>o</sub>'),min=0,max = 1,value = 0.5,step = 0.05,width = '150px'),
                                                                                    numericInput(inputId = 'PropEstim',label = HTML('Inserte Proporción Estimada <i>p&#770;</i>'),min=0,max = 1,value = 0.5,step = 0.05,width = '150px')))


    )
  )
)



server <- function(input, output,session) {

  output$grafica1<-renderPlot({

    mu<-input$MediaHip
    x_bar<-input$MediaMuestral
    n<-input$Muestra
    sigma<-sqrt(input$VarianzaPob)
    alpha<-input$signif

    z<-(x_bar-mu)*sqrt(n)/sigma

    x<-if(-6<=z & z<=6){
      seq(-6,6,0.01)
    } else if(-6>z){
      seq(z-1,6,0.01)
    } else if(z>6){
      seq(-6,z+1,0.01)
    }

    y<-dnorm(x,mean=0, sd=1)


    if(input$tp=='Dos colas'){

    alpha_2<-alpha/2

    z_alpha1<-qnorm(alpha_2,mean=0,sd=1)

    z_alpha2<-qnorm(1-alpha_2,mean=0,sd=1)

    f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
      geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
      geom_area(mapping = aes(x=ifelse(x>=z_alpha2,x,NA),y=ifelse(x>=z_alpha2,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+
      geom_area(mapping = aes(x=ifelse(x<=z_alpha1,x,NA),y=ifelse(x<=z_alpha1,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

      geom_segment(aes(x = z_alpha1, y =0 , xend = z_alpha1, yend = dnorm(z_alpha1,mean=0, sd=1)), colour = "black",linetype=2)+
      geom_segment(aes(x = z_alpha2, y =0 , xend = z_alpha2, yend = dnorm(z_alpha2,mean=0, sd=1)), colour = "black",linetype=2)+
      geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

      annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
      annotate("text", x=z_alpha1, y =-0.02, label="-'Z'[alpha/2]", parse = TRUE)+
      annotate("text", x=z_alpha2, y =-0.02, label="'Z'[alpha/2]", parse = TRUE)+
      annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
      annotate("text", x=z_alpha1-2, y=dnorm(z_alpha1,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+
      annotate("text", x=z_alpha2+2, y=dnorm(z_alpha2,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

      ylim(-0.05,0.41)+
      labs( title = "Prueba de dos colas Distribución Normal",
            x = " ", y = " ",caption = "http://synergy.vision/" )

    return(f)
    }
    else if(input$tp=='Cola superior'){

      z_alpha2<-qnorm(1-alpha,mean=0,sd=1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=z_alpha2,x,NA),y=ifelse(x>=z_alpha2,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = z_alpha2, y =0 , xend = z_alpha2, yend = dnorm(z_alpha2,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

        annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
        annotate("text", x=z_alpha2, y =-0.02, label="'Z'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha2+2, y=dnorm(z_alpha2,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de cola superior Distribución Normal",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)

    }
    else if(input$tp=='Cola inferior'){
      z_alpha1<-qnorm(alpha,mean=0,sd=1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=z_alpha1,x,NA),y=ifelse(x<=z_alpha1,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = z_alpha1, y =0 , xend = z_alpha1, yend = dnorm(z_alpha1,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

        annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
        annotate("text", x=z_alpha1, y =-0.02, label="-'Z'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha1-2, y=dnorm(z_alpha1,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución Normal",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
  })


  output$grafica2<-renderPlot({

    mu<-input$MediaHip1
    x_bar<-input$MediaMuestral1
    n<-input$Muestra1
    sigma<-sqrt(input$Varianzamu1)
    alpha<-input$signif1

    t<-(x_bar-mu)/(sigma/sqrt(n))

    x<-if(-6<=t & t<=6){
      seq(-6,6,0.01)
    } else if(-6>t){
      seq(t-1,6,0.01)
    } else if(t>6){
      seq(-6,t+1,0.01)
    }

    y<-dt(x,df=n-1)


    if(input$tp1=='Dos colas'){

      alpha_2<-alpha/2

      t_alpha1<-qt(alpha_2,df=n-1)

      t_alpha2<-qt(1-alpha_2,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=t_alpha2,x,NA),y=ifelse(x>=t_alpha2,dt(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+
        geom_area(mapping = aes(x=ifelse(x<=t_alpha1,x,NA),y=ifelse(x<=t_alpha1,dt(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha1, y =0 , xend = t_alpha1, yend = dt(t_alpha1,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = t_alpha2, y =0 , xend = t_alpha2, yend = dt(t_alpha2,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha1, y =-0.02, label="-'T'[alpha/2]", parse = TRUE)+
        annotate("text", x=t_alpha2, y =-0.02, label="'T'[alpha/2]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha1-2, y=dt(t_alpha1,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha2+2, y=dt(t_alpha2,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp1=='Cola superior'){

      t_alpha2<-qt(1-alpha,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=t_alpha2,x,NA),y=ifelse(x>=t_alpha2,dt(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha2, y =0 , xend = t_alpha2, yend = dt(t_alpha2,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha2, y =-0.02, label="'T'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha2+2, y=dt(t_alpha2,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de cola superior Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)

    }
    else if(input$tp1=='Cola inferior'){
      t_alpha1<-qt(alpha,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=t_alpha1,x,NA),y=ifelse(x<=t_alpha1,dt(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha1, y =0 , xend = t_alpha1, yend = dt(t_alpha1,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha1, y =-0.02, label="-'T'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha1-2, y=dt(t_alpha1,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
  })

  output$grafica3<-renderPlot({

    mux<-input$Media1Hip
    muy<-input$Media2Hip
    x_bar<-input$Media1Muestral
    y_bar<-input$Media2Muestral
    nx<-input$Muestra11
    ny<-input$Muestra12
    sigma2x<-input$Varianza1Pob
    sigma2y<-input$Varianza2Pob
    alpha<-input$signif2

    z<-((x_bar - y_bar)-(mux-muy))/(sqrt((sigma2x/nx) + (sigma2y/ny)))

    x<-if(-6<=z & z<=6){
      seq(-6,6,0.01)
    } else if(-6>z){
      seq(z-1,6,0.01)
    } else if(z>6){
      seq(-6,z+1,0.01)
    }

    y<-dnorm(x,mean=0, sd=1)

    if(input$tp11=='Dos colas'){

      alpha_2<-alpha/2

      z_alpha1<-qnorm(alpha_2,mean=0,sd=1)

      z_alpha2<-qnorm(1-alpha_2,mean=0,sd=1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=z_alpha2,x,NA),y=ifelse(x>=z_alpha2,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+
        geom_area(mapping = aes(x=ifelse(x<=z_alpha1,x,NA),y=ifelse(x<=z_alpha1,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = z_alpha1, y =0 , xend = z_alpha1, yend = dnorm(z_alpha1,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z_alpha2, y =0 , xend = z_alpha2, yend = dnorm(z_alpha2,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

        annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
        annotate("text", x=z_alpha1, y =-0.02, label="-'Z'[alpha/2]", parse = TRUE)+
        annotate("text", x=z_alpha2, y =-0.02, label="'Z'[alpha/2]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha1-2, y=dnorm(z_alpha1,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha2+2, y=dnorm(z_alpha2,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución Normal",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp11=='Cola superior'){

      z_alpha2<-qnorm(1-alpha,mean=0,sd=1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=z_alpha2,x,NA),y=ifelse(x>=z_alpha2,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = z_alpha2, y =0 , xend = z_alpha2, yend = dnorm(z_alpha2,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

        annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
        annotate("text", x=z_alpha2, y =-0.02, label="'Z'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha2+2, y=dnorm(z_alpha2,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de cola superior Distribución Normal",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp11=='Cola inferior'){
      z_alpha1<-qnorm(alpha,mean=0,sd=1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=z_alpha1,x,NA),y=ifelse(x<=z_alpha1,dnorm(x,mean=0, sd=1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = z_alpha1, y =0 , xend = z_alpha1, yend = dnorm(z_alpha1,mean=0, sd=1)), colour = "black",linetype=2)+
        geom_segment(aes(x = z, y =0 , xend = z, yend = dnorm(z,mean=0, sd=1)), colour = "red",linetype=1)+

        annotate("text", x=z, y =-0.02, label ="Z", parse = TRUE)+
        annotate("text", x=z_alpha1, y =-0.02, label="-'Z'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=z_alpha1-2, y=dnorm(z_alpha1,mean=0, sd=1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución Normal",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }

  })

  output$grafica4<-renderPlot({

    mux<-input$Media1Hip1
    muy<-input$Media2Hip1
    x_bar<-input$Media1Muestral1
    y_bar<-input$Media2Muestral1
    nx<-input$Muestra13
    ny<-input$Muestra14
    sigma2x<-input$Varianza1Mu1
    sigma2y<-input$Varianza2Mu2
    alpha<-input$signif3

    s<-((nx-1)*sigma2x+(ny-1)*sigma2y)/(nx+ny-2)

    t<-((x_bar - y_bar)-(mux-muy))/(s*sqrt((1/nx) + (1/ny)))

    x<-if(-6<=t & t<=6){
      seq(-6,6,0.01)
    } else if(-6>t){
      seq(t-1,6,0.01)
    } else if(t>6){
      seq(-6,t+1,0.01)
    }

    y<-dt(x,df=nx+ny-2)

    if(input$tp12=='Dos colas'){

      alpha_2<-alpha/2

      t_alpha1<-qt(alpha_2,df=nx+ny-2)

      t_alpha2<-qt(1-alpha_2,df=nx+ny-2)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=t_alpha2,x,NA),y=ifelse(x>=t_alpha2,dt(x,df=nx+ny-2),NA)), fill = "blue",alpha = 0.4)+
        geom_area(mapping = aes(x=ifelse(x<=t_alpha1,x,NA),y=ifelse(x<=t_alpha1,dt(x,df=nx+ny-2),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha1, y =0 , xend = t_alpha1, yend = dt(t_alpha1,df=nx+ny-2)), colour = "black",linetype=2)+
        geom_segment(aes(x = t_alpha2, y =0 , xend = t_alpha2, yend = dt(t_alpha2,df=nx+ny-2)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=nx+ny-2)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha1, y =-0.02, label="-'T'[alpha/2]", parse = TRUE)+
        annotate("text", x=t_alpha2, y =-0.02, label="'T'[alpha/2]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha1-2, y=dt(t_alpha1,df=nx+ny-2), label="'Rechazar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha2+2, y=dt(t_alpha2,df=nx+ny-2), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp12=='Cola superior'){

      t_alpha2<-qt(1-alpha,df=nx+ny-2)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=t_alpha2,x,NA),y=ifelse(x>=t_alpha2,dt(x,df=nx+ny-2),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha2, y =0 , xend = t_alpha2, yend = dt(t_alpha2,df=nx+ny-2)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=nx+ny-2)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha2, y =-0.02, label="'T'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha2+2, y=dt(t_alpha2,df=nx+ny-2), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de cola superior Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp12=='Cola inferior'){
      t_alpha1<-qt(alpha,df=nx+ny-2)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=t_alpha1,x,NA),y=ifelse(x<=t_alpha1,dt(x,df=nx+ny-2),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = t_alpha1, y =0 , xend = t_alpha1, yend = dt(t_alpha1,df=nx+ny-2)), colour = "black",linetype=2)+
        geom_segment(aes(x = t, y =0 , xend = t, yend = dt(t,df=nx+ny-2)), colour = "red",linetype=1)+

        annotate("text", x=t, y =-0.02, label ="T", parse = TRUE)+
        annotate("text", x=t_alpha1, y =-0.02, label="-'T'[alpha]", parse = TRUE)+
        annotate("text", x=0, y = 0.1, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=t_alpha1-2, y=dt(t_alpha1,df=nx+ny-2), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,0.41)+
        labs( title = "Prueba de dos colas Distribución T-student",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }

  })

  output$grafica5<-renderPlot({

    sigma2o<-input$SigmaHip
    S2<-input$SigmaMuestral
    n<-input$MuestraVar
    alpha<-input$signif4

    X2<-((n-1)*S2)/sigma2o

    p<-n*2.5

    x<-if(0<=X2 & X2<=p){
      seq(0,p,0.01)
    } else if(0>X2){
      seq(X2-1,p,0.01)
    } else if(X2>p){
      seq(0,X2+1,0.01)
    }

    y<-dchisq(x,df=n-1)

    if(input$tp2=='Dos colas'){

      alpha_2<-alpha/2

      x_alpha1<-qchisq(alpha_2,df=n-1)

      x_alpha2<-qchisq(1-alpha_2,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=x_alpha2,x,NA),y=ifelse(x>=x_alpha2,dchisq(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+
        geom_area(mapping = aes(x=ifelse(x<=x_alpha1,x,NA),y=ifelse(x<=x_alpha1,dchisq(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = x_alpha1, y =0 , xend = x_alpha1, yend = dchisq(x_alpha1,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = x_alpha2, y =0 , xend = x_alpha2, yend = dchisq(x_alpha2,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = X2, y =0 , xend = X2, yend = dchisq(X2,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=X2, y =-0.003, label ="X2", parse = TRUE)+
        annotate("text", x=x_alpha1, y =-0.003, label="-'X'[alpha/2]", parse = TRUE)+
        annotate("text", x=x_alpha2, y =-0.003, label="'X'[alpha/2]", parse = TRUE)+
        annotate("text", x=n, y = dchisq(n,n-1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=x_alpha1-1, y=dchisq(x_alpha1,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+
        annotate("text", x=x_alpha2+1, y=dchisq(x_alpha2,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        labs( title = "Prueba de dos colas Distribución chi cuadrado",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)
    }
    else if(input$tp2=='Cola superior'){

      x_alpha2<-qchisq(1-alpha,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=x_alpha2,x,NA),y=ifelse(x>=x_alpha2,dchisq(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = x_alpha2, y =0 , xend = x_alpha2, yend = dchisq(x_alpha2,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = X2, y =0 , xend = X2, yend = dchisq(X2,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=X2, y =-0.003, label ="X2", parse = TRUE)+
        annotate("text", x=x_alpha2, y =-0.003, label="'X'[alpha]", parse = TRUE)+
        annotate("text", x=n, y = dchisq(n,n-1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=x_alpha2+1, y=dchisq(x_alpha2,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        labs( title = "Prueba cola superior Distribución chi cuadrado",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)

    }
    else if(input$tp2=='Cola inferior'){

      x_alpha1<-qchisq(alpha,df=n-1)

      f<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=x_alpha1,x,NA),y=ifelse(x<=x_alpha1,dchisq(x,df=n-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = x_alpha1, y =0 , xend = x_alpha1, yend = dchisq(x_alpha1,df=n-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = X2, y =0 , xend = X2, yend = dchisq(X2,df=n-1)), colour = "red",linetype=1)+

        annotate("text", x=X2, y =-0.003, label ="X2", parse = TRUE)+
        annotate("text", x=x_alpha1, y =-0.003, label="-'X'[alpha]", parse = TRUE)+
        annotate("text", x=n, y = dchisq(n,n-1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=x_alpha1-1, y=dchisq(x_alpha1,df=n-1), label="'Rechazar H'[0]", parse = TRUE)+

        labs( title = "Prueba cola inferior Distribución chi cuadrado",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f)

    }

  })

  output$grafica6<-renderPlot({

    varx<-input$var1Hip1
    vary<-input$var2Hip1
    nx<-input$Muestra4
    ny<-input$Muestra5
    sigma2x<-input$var1Muestral1
    sigma2y<-input$var2Muestral1
    alpha<-input$signif5

    f<-(sigma2x/varx)/(sigma2y/vary)


    x<-seq(0,10,0.01)

    y<-df(x, df1= nx - 1,df2 = ny - 1)

    if(input$tp3=='Dos colas'){

      alpha_2<-alpha/2

      f_alpha1<-qf(alpha_2,df1 = nx - 1,df2 = ny - 1)

      f_alpha2<-qf(1-alpha_2,df1=nx - 1,df2 = ny - 1)

      f1<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=f_alpha2,x,NA),y=ifelse(x>=f_alpha2,df(x,df1= nx - 1,df2= ny - 1),NA)), fill = "blue",alpha = 0.4)+
        geom_area(mapping = aes(x=ifelse(x<=f_alpha1,x,NA),y=ifelse(x<=f_alpha1,df(x,df1= nx - 1,df2=ny - 1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = f_alpha1, y =0 , xend = f_alpha1, yend = df(f_alpha1,df1= nx - 1,df2= ny - 1)), colour = "black",linetype=2)+
        geom_segment(aes(x = f_alpha2, y =0 , xend = f_alpha2, yend = df(f_alpha2,df1= nx - 1,df2= ny - 1)), colour = "black",linetype=2)+
        geom_segment(aes(x = f, y =0 , xend = f, yend = df(f,df1= nx - 1,df2= ny - 1)), colour = "red",linetype=1)+

        annotate("text", x=f, y =-0.02, label ="F", parse = TRUE)+
        annotate("text", x=f_alpha1, y =-0.02, label="-'F'[alpha/2]", parse = TRUE)+
        annotate("text", x=f_alpha2, y =-0.02, label="'F'[alpha/2]", parse = TRUE)+
        annotate("text", x=ny/(ny-2), y = df(ny/(ny-2),df1 = nx - 1 ,df2 = ny - 1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=f_alpha1-0.5, y=df(f_alpha1,df1= nx - 1,df2= ny - 1), label="'Rechazar H'[0]", parse = TRUE)+
        annotate("text", x=f_alpha2+0.5, y=df(f_alpha2,df1= nx - 1,df2= ny - 1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,max(y)+0.01)+
        labs( title = "Prueba de dos colas Distribución Fisher",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f1)
    }
    else if(input$tp3=='Cola superior'){

      f_alpha2<-qf(1-alpha,df1=nx-1,df2 = ny-1)

      f1<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x>=f_alpha2,x,NA),y=ifelse(x>=f_alpha2,df(x,df1=nx-1,df2=ny-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = f_alpha2, y =0 , xend = f_alpha2, yend = df(f_alpha2,df1=nx-1,df2=ny-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = f, y =0 , xend = f, yend = df(f,df1=nx-1,df2=ny-1)), colour = "red",linetype=1)+

        annotate("text", x=f, y =-0.02, label ="F", parse = TRUE)+
        annotate("text", x=f_alpha2, y =-0.02, label="'F'[alpha]", parse = TRUE)+
        annotate("text", x=ny/(ny-2), y = df(ny/(ny-2),df1=nx-1,df2=ny-1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=f_alpha2+0.5, y=df(f_alpha2,df1=nx-1,df2=ny-1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,max(y)+0.01)+
        labs( title = "Prueba de cola superior Distribución Fisher",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f1)
    }
    else if(input$tp3=='Cola inferior'){
      f_alpha1<-qf(alpha,df1=nx-1,df2=ny-1)

      f1<-ggplot(mapping = aes(x,y))+geom_line(colour = "blue")+
        geom_area(mapping = aes(x,y), fill = "blue",alpha = 0.2)+
        geom_area(mapping = aes(x=ifelse(x<=f_alpha1,x,NA),y=ifelse(x<=f_alpha1,df(x,df1=nx-1,df2=ny-1),NA)), fill = "blue",alpha = 0.4)+

        geom_segment(aes(x = f_alpha1, y =0 , xend = f_alpha1, yend = df(f_alpha1,df1=nx-1,df2=ny-1)), colour = "black",linetype=2)+
        geom_segment(aes(x = f, y =0 , xend = f, yend = df(f,df1=nx-1,df2=ny-1)), colour = "red",linetype=1)+

        annotate("text", x=f, y =-0.02, label ="F", parse = TRUE)+
        annotate("text", x=f_alpha1, y =-0.02, label="-'F'[alpha]", parse = TRUE)+
        annotate("text", x=ny/(ny-2), y = df(ny/(ny-2),df1=nx-1,df2 = ny-1)/2, label="'Aceptar H'[0]", parse = TRUE)+
        annotate("text", x=f_alpha1-0.5, y=df(f_alpha1,df1=nx-1,df2=ny-1), label="'Rechazar H'[0]", parse = TRUE)+

        ylim(-0.05,max(y)+0.01)+
        labs( title = "Prueba de cola inferior Distribución Fisher",
              x = " ", y = " ",caption = "http://synergy.vision/" )

      return(f1)
    }

})









  }


shinyApp(ui = ui, server = server)
