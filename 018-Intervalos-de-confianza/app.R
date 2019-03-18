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

  titlePanel("Intervalos de confianza"),
  sidebarLayout(

    sidebarPanel(width = 3,

  selectInput(inputId = 'ic',label = 'Escoja el intervalo de confianza deseado',choices = c('Media de una población','Proporción en una población','Varianza de una población','Diferencia de medias de dos poblaciones'),
              selected = NULL),
  conditionalPanel(condition = "input.ic=='Media de una población'",selectInput(inputId = 'vc',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL)),
  conditionalPanel(condition = "input.ic=='Diferencia de medias de dos poblaciones'",selectInput(inputId = 'vc1',label = '',choices = c('Varianza conocida','Varianza desconocida'),selected = NULL))
    ),

  mainPanel(withMathJax(),width = 9,
            conditionalPanel(condition = "input.ic=='Media de una población' & input.vc=='Varianza conocida'",column(width = 3,numericInput(inputId = 'MediaMuestral',label = HTML('Inserte media de la muestra X&#772;'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'Muestra',label = HTML('Inserte tamaño de la muestra <i>n</i>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'VarianzaPob',label = HTML('Inserte varianza poblacional <i>&sigma;<sup>2</sup></i>'),min=0.1,max = 50,value = 1,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'signif',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo'),plotOutput('grafica1'))),

            conditionalPanel(condition = "input.ic=='Media de una población' & input.vc=='Varianza desconocida'",column(width = 3,numericInput(inputId = 'MediaMuestral1',label = HTML('Inserte media de la muestra X&#772;'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'Muestra1',label = HTML('Inserte tamaño de la muestra <i>n</i>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'VarianzaPob1',label = HTML('Inserte varianza muestral <i>S<sup>2</sup></i>'),min=0.1,max = 50,value = 1,step = 1,width = '150px'),
                                                                                                                     numericInput(inputId = 'signif1',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo1'),plotOutput('grafica2'))),

            conditionalPanel(condition = "input.ic=='Proporción en una población'",column(width = 3,numericInput(inputId = 'Prop',label = HTML('Inserte proporción <i>p&#770;</i>'),min=0,max = 1,value = 0.6,step = 0.05,width = '150px'),
                                                                                          numericInput(inputId = 'Muestra2',label = HTML('Inserte tamaño de la muestra <i>n</i>'),min=30,max = 500,value = 50,step = 1,width = '150px'),
                                                                                          numericInput(inputId = 'signif2',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo2'),plotOutput('grafica3'))),

            conditionalPanel(condition = "input.ic=='Varianza de una población'",column(width = 3,numericInput(inputId = 'MuestraVar',label = HTML('Inserte varianza muestral <i>S<sup>2</sup></i>'),min=0.1,max = 50,value = 5,step = 1,width = '150px'),
                                                                                        numericInput(inputId = 'Muestra3',label = HTML('Inserte tamaño de la muestra <i>n</i>'),min=1,max = 250,value = 50,step = 1,width = '150px'),
                                                                                        numericInput(inputId = 'signif3',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo3'),plotOutput('grafica4'))),

            conditionalPanel(condition = "input.ic=='Diferencia de medias de dos poblaciones' & input.vc1=='Varianza conocida'",column(width = 2,numericInput(inputId = 'MediaMuestral2',label = HTML('Inserte media de la muestra X&#772;<sub>1</sub>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'MediaMuestral3',label = HTML('Inserte media de la muestra X&#772;<sub>2</sub>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'Muestra4',label = HTML('Inserte tamaño de la muestra <i>n</i><sub>1</sub>'),min=1,max = 250,value = 50,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'Muestra5',label = HTML('Inserte tamaño de la muestra <i>n</i><sub>2</sub>'),min=1,max = 250,value = 50,step = 1,width = '150px')),

                             column(width = 2,br(),numericInput(inputId = 'VarianzaPob2',label = HTML('Inserte varianza poblacional <i>&sigma;<sup>2</sup><sub>1</sub></i>'),min=0.1,max = 50,value = 1,step = 1,width = '150px'),
                                    numericInput(inputId = 'VarianzaPob3',label = HTML('Inserte varianza poblacional <i>&sigma;<sup>2</sup><sub>2</sub></i>'),min=0.1,max = 50,value = 4,step = 1,width = '150px'),
                                    numericInput(inputId = 'signif4',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo4'),plotOutput('grafica5'))),

            conditionalPanel(condition = "input.ic=='Diferencia de medias de dos poblaciones' & input.vc1=='Varianza desconocida'",column(width = 2,numericInput(inputId = 'MediaMuestral4',label = HTML('Inserte media de la muestra X&#772;<sub>1</sub>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'MediaMuestral5',label = HTML('Inserte media de la muestra X&#772;<sub>2</sub>'),min=0,max = 100,value = 5,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'Muestra6',label = HTML('Inserte tamaño de la muestra <i>n</i><sub>1</sub>'),min=1,max = 250,value = 50,step = 1,width = '150px'),
                                                                                                                                       numericInput(inputId = 'Muestra7',label = HTML('Inserte tamaño de la muestra <i>n</i><sub>2</sub>'),min=1,max = 250,value = 50,step = 1,width = '150px')),

                             column(width = 2,br(),numericInput(inputId = 'VarianzaPob4',label = HTML('Inserte varianza muestral <i>S<sup>2</sup><sub>1</sub></i>'),min=0.1,max = 50,value = 1,step = 1,width = '150px'),
                                    numericInput(inputId = 'VarianzaPob5',label = HTML('Inserte varianza muestral <i>S<sup>2</sup><sub>2</sub></i>'),min=0.1,max = 50,value = 4,step = 1,width = '150px'),
                                    numericInput(inputId = 'signif5',label = HTML('Inserte nivel de significancia <i>&alpha;</i>'),min=0.01,max = 0.1,value = 0.05,step = 0.01,width = '150px')),
                             column(width = 8,align='center',uiOutput('intervalo5'),plotOutput('grafica6')))
    )
  )
)



server <- function(input, output,session) {

  output$intervalo<-renderUI({

    x_bar<-input$MediaMuestral
    n<-input$Muestra
    sigma<-sqrt(input$VarianzaPob)
    alpha<-input$signif

    alpha_2<-alpha/2

    z_alpha2<-qnorm(1-alpha_2,0,1)

    ic<-round(z_alpha2*(sigma/sqrt(n)),2)

    left<-x_bar-ic
    right<-x_bar+ic

    p1<-paste(left,right,sep='  ,  ')
    p2<-paste('[',p1,sep='')
    p3<-paste(p2,']',sep='')

    return(h2(p3))

  })


  output$grafica1<-renderPlot({

    x_bar<-input$MediaMuestral
    n<-input$Muestra
    sigma<-sqrt(input$VarianzaPob)
    alpha<-input$signif

    alpha_2<-alpha/2

    z_alpha2<-qnorm(1-alpha_2,0,1)

    ic<-round(z_alpha2*(sigma/sqrt(n)),2)

    left<-x_bar-ic
    right<-x_bar+ic

    f<-ggplot()+
      geom_curve( aes(x = (x_bar-10), y = 0, xend = (x_bar+10), yend = 0),curvature = 0)+
      #scale_x_continuous(breaks = seq(28000,38000,by=2000)) +xlab('')+ylab('')+
      annotate("text", x=x_bar, y=-0.005, label=paste(x_bar),parse = TRUE)+ylim(-0.05,0.05)+
      annotate("text",x=left-2,y=-0.005,label=paste(x_bar,ic,sep = ' - '),parse=TRUE)+
      annotate("text",x=left-2,y=0,label="'('",parse=TRUE,col='red',size=8)+
      annotate("text",x=right+2,y=-0.005,label=paste(x_bar,ic,sep = ' + '),parse=TRUE)+
      annotate("text",x=right+2,y=0,label="')'",parse=TRUE,col='red',size=8)+
      geom_segment(aes(x = x_bar, y =-0.002 , xend = x_bar,yend = 0),colour = "black",linetype=2)+
      labs( title = "Intervalo de confianza",
            x = " ", y = " ",caption = "https://synergy.vision/" )


    return(f)


  })

  output$intervalo1<-renderUI({

    x_bar<-input$MediaMuestral1
    n<-input$Muestra1
    s<-sqrt(input$VarianzaPob1)
    alpha<-input$signif1

    alpha_2<-alpha/2

    t_alpha2<-qt(1-alpha_2,df = n-1)

    ic<-round(t_alpha2*(s/sqrt(n)),2)

    left<-x_bar-ic
    right<-x_bar+ic

    p1<-paste(left,right,sep='  ,  ')
    p2<-paste('[',p1,sep='')
    p3<-paste(p2,']',sep='')

    return(h2(p3))

  })


  output$grafica2<-renderPlot({

    x_bar<-input$MediaMuestral1
    n<-input$Muestra1
    s<-sqrt(input$VarianzaPob1)
    alpha<-input$signif1

    alpha_2<-alpha/2

    t_alpha2<-qt(1-alpha_2,df=n-1)

    ic<-round(t_alpha2*(s/sqrt(n)),2)

    left<-x_bar-ic
    right<-x_bar+ic

    f<-ggplot()+
      geom_curve( aes(x = (x_bar-10), y = 0, xend = (x_bar+10), yend = 0),curvature = 0)+
      #scale_x_continuous(breaks = seq(28000,38000,by=2000)) +xlab('')+ylab('')+
      annotate("text", x=x_bar, y=-0.005, label=paste(x_bar),parse = TRUE)+ylim(-0.05,0.05)+
      annotate("text",x=left-2,y=-0.005,label=paste(x_bar,ic,sep = ' - '),parse=TRUE)+
      annotate("text",x=left-2,y=0,label="'('",parse=TRUE,col='red',size=8)+
      annotate("text",x=right+2,y=-0.005,label=paste(x_bar,ic,sep = ' + '),parse=TRUE)+
      annotate("text",x=right+2,y=0,label="')'",parse=TRUE,col='red',size=8)+
      geom_segment(aes(x = x_bar, y =-0.002 , xend = x_bar,yend = 0),colour = "black",linetype=2)+
      labs( title = "Intervalo de confianza",
            x = " ", y = " ",caption = "https://synergy.vision/" )


    return(f)


  })

  output$intervalo2<-renderUI({

    p<-input$Prop
    n<-input$Muestra2
    alpha<-input$signif2

    alpha_2<-alpha/2

    z_alpha2<-qnorm(1-alpha_2,0,1)

    ic<-round(z_alpha2*sqrt(p*(1-p)/n),2)

    left<-p-ic
    right<-p+ic

    p1<-paste(left,right,sep='  ,  ')
    p2<-paste('[',p1,sep='')
    p3<-paste(p2,']',sep='')

    return(h2(p3))

  })


  output$grafica3<-renderPlot({

    p<-input$Prop
    n<-input$Muestra2
    alpha<-input$signif2

    alpha_2<-alpha/2

    z_alpha2<-qnorm(1-alpha_2,0,1)

    ic<-round(z_alpha2*sqrt(p*(1-p)/n),2)

    left<-p-ic
    right<-p+ic

    f<-ggplot()+
      geom_curve( aes(x = (p-10), y = 0, xend = (p+10), yend = 0),curvature = 0)+
      #scale_x_continuous(breaks = seq(28000,38000,by=2000)) +xlab('')+ylab('')+
      annotate("text", x=p, y=-0.005, label=paste(p),parse = TRUE)+ylim(-0.05,0.05)+
      annotate("text",x=left-2,y=-0.005,label=paste(p,ic,sep = ' - '),parse=TRUE)+
      annotate("text",x=left-2,y=0,label="'('",parse=TRUE,col='red',size=8)+
      annotate("text",x=right+2,y=-0.005,label=paste(p,ic,sep = ' + '),parse=TRUE)+
      annotate("text",x=right+2,y=0,label="')'",parse=TRUE,col='red',size=8)+
      geom_segment(aes(x = p, y =-0.002 , xend = p,yend = 0),colour = "black",linetype=2)+
      labs( title = "Intervalo de confianza",
            x = " ", y = " ",caption = "https://synergy.vision/" )


    return(f)


  })

  output$intervalo3<-renderUI({

    s2<-input$MuestraVar
    n<-input$Muestra3
    alpha<-input$signif3

    alpha_2<-alpha/2

    ix_alpha2<-qchisq(1-alpha_2,df=n-1)
    ux_alpha2<-qchisq(alpha_2,df=n-1)



    left<-round((n-1)*s2/ix_alpha2,2)
    right<-round((n-1)*s2/ux_alpha2,2)

    p1<-paste(left,right,sep='  ,  ')
    p2<-paste('[',p1,sep='')
    p3<-paste(p2,']',sep='')

    return(h2(p3))

  })


  output$grafica4<-renderPlot({

    s2<-input$MuestraVar
    n<-input$Muestra3
    alpha<-input$signif3

    alpha_2<-alpha/2

    ix_alpha2<-qchisq(1-alpha_2,df=n-1)
    ux_alpha2<-qchisq(alpha_2,df=n-1)



    left<-round((n-1)*s2/ix_alpha2,2)
    right<-round((n-1)*s2/ux_alpha2,2)

    f<-ggplot()+
      geom_curve( aes(x = (left-10), y = 0, xend = (right+10), yend = 0),curvature = 0)+
      #scale_x_continuous(breaks = seq(28000,38000,by=2000)) +xlab('')+ylab('')+
      annotate("text", x=s2, y=-0.005, label=paste(s2),parse = TRUE)+ylim(-0.05,0.05)+
      annotate("text",x=left-2,y=-0.005,label=paste(left),parse=TRUE)+
      annotate("text",x=left-2,y=0,label="'('",parse=TRUE,col='red',size=8)+
      annotate("text",x=right+2,y=-0.005,label=paste(right),parse=TRUE)+
      annotate("text",x=right+2,y=0,label="')'",parse=TRUE,col='red',size=8)+
      geom_segment(aes(x = s2, y =-0.002 , xend = s2,yend = 0),colour = "black",linetype=2)+
      labs( title = "Intervalo de confianza",
            x = " ", y = " ",caption = "https://synergy.vision/" )


    return(f)


  })


  }


shinyApp(ui = ui, server = server)
