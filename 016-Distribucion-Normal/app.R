ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.2.0")
ensure_version("readxl", "1.2.0")
ensure_version("shinydasboard", "0.7.1")
ensure_version("psych", "1.8.10")
#ensure_version("modeest", "2.3.2")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.1.0")




library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
#library(modeest)
library(matrixStats)
library(ggplot2)


ui <- fluidPage(
  
  titlePanel("Distribución Normal"),
  tabsetPanel(type = 'pills',id='pri',
              tabPanel('Características',includeHTML("normal.html")),
              tabPanel('Cálculos',br(),column(width=5,selectInput(inputId = 'nor',label = HTML('Seleccione el cálculo deseado'),choices = c('Función de Densidad','Función de Distribución','Comparación de Medias','Comparación de Varianzas','Cuantiles','Muestra Aleatoria','Probabilidad por rango'),selected = NULL),
                       conditionalPanel(condition = "input.nor=='Función de Densidad'",
                                        numericInput(inputId = 'mu',label = HTML('Seleccione el valor de la media &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor',label = HTML('Seleccione el valor de la función de densidad'),min=-50,max=50,step=0.1,value = 5,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Función de Distribución'",
                                        numericInput(inputId = 'mu1',label = HTML('Seleccione el valor del parámetro &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma1',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor1',label = HTML('Seleccione el valor de la función de distribución'),min=-50,max=50,step=0.1,value = 5,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Comparación de Medias'",
                                        numericInput(inputId = 'mu_1',label = HTML('Seleccione el valor del parámetro &mu;<sub>1</sub>'),min=-10,max=10,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'mu_2',label = HTML('Seleccione el valor del parámetro &mu;<sub>2</sub>'),min=-10,max=10,step=0.1,value = 3,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Comparación de Varianzas'",
                                        numericInput(inputId = 'va_1',label = HTML('Seleccione el valor del parámetro &sigma;<sub>1</sub>'),min=-10,max=10,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'va_2',label = HTML('Seleccione el valor del parámetro &sigma;<sub>2</sub>'),min=-10,max=10,step=0.1,value = 3,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Cuantiles'",
                                        numericInput(inputId = 'mu2',label = HTML('Seleccione el valor del parámetro &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma2',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor2',label = HTML('Seleccione la probabilidad asociada al cuantil'),min=0,max=1,step=0.1,value = 0.5,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Muestra Aleatoria'",
                                        numericInput(inputId = 'mu3',label = HTML('Seleccione el valor del parámetro &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma3',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        numericInput(inputId = 'valor3',label = HTML('Seleccione el tamaño de la muestra deseada'),min=1,max=200,step=1,value = 20,width = '150px')),
                       conditionalPanel(condition = "input.nor=='Probabilidad por rango'",
                                        numericInput(inputId = 'mu4',label = HTML('Seleccione el valor de la media &mu;'),min=-20,max=20,step=0.1,value = 0,width = '150px'),
                                        numericInput(inputId = 'sigma4',label = HTML('Seleccione el valor de la desviación estándar &sigma;'),min=0.1,max=20,step=0.1,value = 1,width = '150px'),
                                        radioButtons(inputId = 'prob',label = 'Elija la probabilidad a calcular',choiceNames = list(tags$i('P(X \\(\\leq\\) a)'),tags$i('P(a \\(\\leq\\) X \\(\\leq\\) b)'),tags$i('P(X \\(\\geq\\) b)')),choiceValues = c('int1','int2','int3'),selected = ''),
                                        conditionalPanel(condition = "input.prob=='int1' ", numericInput(inputId = 'a',label = HTML('Seleccione el valor de a'),min=-20,max=20,step=0.1,value = 0,width = '150px')),
                                        conditionalPanel(condition = "input.prob == 'int2'",numericInput('ab1',label = 'Seleccione el valor de a',min = -20,max=20,value=0,step = 0.1,width = '300px'),
                                                         numericInput('ab2',label = 'Seleccione el valor de b',min = -20,max=20,value=1,step = 0.1,width = '300px')),
                                        conditionalPanel(condition = "input.prob=='int3' ", numericInput(inputId = 'b',label = HTML('Seleccione el valor de b'),min=-20,max=20,step=0.1,value = 0,width = '150px'))
                                        )
              ),
              conditionalPanel(condition = "input.nor=='Función de Densidad'",column(width=7,align='center',br(),verbatimTextOutput("norm"),plotOutput("densnor"))),
              conditionalPanel(condition = "input.nor=='Función de Distribución'",column(width=6,align='center',br(),verbatimTextOutput("norm1"),plotOutput("densnor1"))),
              conditionalPanel(condition = "input.nor=='Comparación de Medias'",column(width=7,align='center',br(),plotOutput("densnor2"))),
              conditionalPanel(condition = "input.nor=='Comparación de Varianzas'",column(width=7,align='center',br(),plotOutput("densnor3"))),
              conditionalPanel(condition = "input.nor=='Cuantiles'",column(width=6,align='center',br(),verbatimTextOutput("norm2"),plotOutput("densnor4"))),
              conditionalPanel(condition = "input.nor=='Muestra Aleatoria'",column(width=6,align='center',br(),verbatimTextOutput("norm3"),plotOutput("densnor5"))),
              conditionalPanel(condition = "input.nor=='Probabilidad por rango' & input.prob=='int1'",column(width=6,align='center',br(),verbatimTextOutput("norm4"),plotOutput("densnor6"))),
              conditionalPanel(condition = "input.nor=='Probabilidad por rango' & input.prob=='int2'",column(width=6,align='center',br(),verbatimTextOutput("norm5"),plotOutput("densnor7"))),
              conditionalPanel(condition = "input.nor=='Probabilidad por rango' & input.prob=='int3'",column(width=6,align='center',br(),verbatimTextOutput("norm6"),plotOutput("densnor8")))
                       ))
)



server <- function(input, output,session) {
  
  output$norm<-renderText({
    x<-input$valor
    media<-input$mu
    dv<-input$sigma
    resultado<-paste("f(",x,") = ", dnorm(x,mean=media,sd=dv))
    return(resultado)
  })
  
  output$densnor<-renderPlot({
    x1<-input$valor
    
    media<-input$mu
    dv<-input$sigma
    
    x <- seq(media-6,6+media,0.01)
    hx <- dnorm(x,mean=media,sd=dv)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dnorm(x1,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  output$norm1<-renderText({
    x<-input$valor1
    media<-input$mu1
    dv<-input$sigma1
    
    resultado<-paste("F(",x,") = P(X <=",x,") = ", pnorm(x,mean = media, sd=dv))
    return(resultado)
  })
  
  
  output$densnor1<-renderPlot({
    x<-input$valor1
    media<-input$mu1
    dv<-input$sigma1
    
    data<-data.frame(norm=pnorm(seq(-x,x,0.01),mean = media,sd=dv))
    
    f1<-ggplot(data,aes(x=seq(-x,x,0.01),y=norm))+geom_line(colour='blue',size=1)+
      labs( title = "Distribución Normal",
            x = "x", y = "F(x)", caption = "http://synergy.vision/" )
    return(f1)
  })
  
  
  output$densnor2<-renderPlot({
  media1<-input$mu_1
  media2<-input$mu_2
  x1<-seq(media1-6,media1+6,0.01)
  x2<-seq(media2-6,media2+6,0.01)
  y1 <- dnorm(x1,mean=media1, sd=1)
  y2 <- dnorm(x2,mean=media2, sd=1)
  
  dat<-data.frame(x1,x2,y1,y2)
  ggplot(data=dat, mapping = aes(x1,y1))+geom_line(aes(colour = I("yellow")))+
    geom_area(mapping = aes(x1,y1), fill = "yellow",alpha = .2)+
    geom_segment(aes(x = media1, y =0 , xend = media1,
                     yend = dnorm(media1,mean = media1,sd=1)),linetype="dashed",colour="yellow")+
    geom_line(data=dat, aes(x2,y2,colour = I("red")))+
    geom_segment(aes(x = media2, y =0 , xend = media2,
                     yend = dnorm(media2,mean = media2,sd=1)),linetype="dashed",colour="red")+
    geom_area(mapping = aes(x2,y2), fill = "red",alpha = .2)+
    labs(title = 'Medias de la distribución Normal',
         x = "x", y = "f(x)")+
    theme(plot.title = element_text(size = rel(1.3),hjust = 0.5))+scale_color_manual('Medias',values=c("red","yellow"),labels=c('Media 2','Media 1'))
  })
  
  output$densnor3<-renderPlot({
    x<-seq(-15,15,0.01)
    y1 <- dnorm(x,mean=0, sd=input$va_1)
    y2 <- dnorm(x,mean=0, sd=input$va_2)
    
    dat<-data.frame(x,y1,y2)
    ggplot(data=dat, mapping = aes(x,y1))+geom_line(aes(colour = I("yellow")))+
      geom_area(mapping = aes(x,y1), fill = "yellow",alpha = .2)+
      geom_line(data=dat, aes(x,y2,colour = I("red")))+
      geom_area(mapping = aes(x,y2), fill = "red",alpha = .2)+
      labs(title = 'Varianzas de la distribución Normal',
           x = "x", y = "f(x)")+
      theme(plot.title = element_text(size = rel(1.3),hjust = 0.5)) +scale_color_manual('Varianzas',values=c("red","yellow"),labels=c('Varianza 2','Varianza 1'))
  })
  
  output$norm2<-renderText({
    x<-input$valor2
    media<-input$mu2
    dv<-input$sigma2
    
    
    resultado<-paste("x = ", qnorm(x,mean = media,sd=dv))
    return(resultado)
  })
  
  output$densnor4<-renderPlot({
    x1<-input$valor2
    media<-input$mu2
    dv<-input$sigma2
    
    x2<-qnorm(x1,mean = media,sd=dv) #cuantil
    x <- seq(media-6,6+media,0.01)
    hx <- dnorm(x,mean=media,sd=dv)
    
    dat<-data.frame(x,hx)
    
    f<-ggplot(data=dat, mapping = aes(x,hx))+geom_line()+
      geom_area(mapping = aes(x), fill = "blue",alpha = 0.4)+
      geom_segment(aes(x = x2, y =0 , xend = x2,
                       yend = dnorm(x2, mean= media ,sd = dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  muestra<-reactive({
    x<-input$valor3
    media<-input$mu3
    dv<-input$sigma3
    
    round(rnorm(x,mean=media,sd=dv),2)
  })
  
  output$norm3<-renderPrint({
    return(muestra())
  })
  
  output$densnor5<-renderPlot({
    data2<-data.frame(x=muestra())
    f2<-ggplot(data2,mapping=aes(x=1:length(x),y=x))+geom_point(colour='blue')+scale_x_continuous(breaks = 1:length(data2$x))+
      labs( title = "Muestra Aleatoria",
            x = "x", y = "m.a.s", caption = "http://synergy.vision/" )
    return(f2)
  })
  
  output$norm4<-renderText({
    x<-input$a
    media<-input$mu4
    dv<-input$sigma4
    resultado<-paste("P(X <=",x,") = ", pnorm(x,mean=media,sd=dv))
    return(resultado)
  })
  
  output$densnor6<-renderPlot({
    x1<-input$a
    media<-input$mu4
    dv<-input$sigma4

    xp <- seq(media-6,6+media,0.01)
    hx <- dnorm(xp,mean=media,sd=dv)

    dat<-data.frame(xp,hx)

    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_line()+
      geom_area(mapping = aes(xp,hx), fill = "blue",alpha = 0.2)+
      geom_area(mapping = aes(x=ifelse(xp<=x1,xp,NA),y=ifelse(xp<=x1,dnorm(xp,mean=media,sd=dv),NA)), fill = "blue",alpha = 0.5)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dnorm(x1,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  output$norm5<-renderText({
    media<-input$mu4
    dv<-input$sigma4
    l<-input$ab1
    u<-input$ab2
    resultado<-paste("P(",l,"<= X <=",u,") = ", pnorm(u,mean = media,sd=dv)-pnorm(l,mean = media,sd=dv))
    return(resultado)
    
  })
  
  output$densnor7<-renderPlot({
    l<-input$ab1
    u<-input$ab2
    media<-input$mu4
    dv<-input$sigma4
    
    xp <- seq(media-6,6+media,0.01)
    hx <- dnorm(xp,mean=media,sd=dv)
    
    dat<-data.frame(xp,hx)
    
    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_line()+
      geom_area(mapping = aes(xp,hx), fill = "blue",alpha = 0.2)+
      geom_area(mapping = aes(x=ifelse(l<=xp & xp<=u,xp,NA),y=ifelse(l<=xp & xp<=u,dnorm(xp,mean=media,sd=dv),NA)), fill = "blue",alpha = 0.5)+
      geom_segment(aes(x = l, y =0 , xend = l,
                       yend = dnorm(l,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      geom_segment(aes(x = u, y =0 , xend = u,
                       yend = dnorm(u,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  output$norm6<-renderText({
    x<-input$b
    media<-input$mu4
    dv<-input$sigma4
    resultado<-paste("P(X >=",x,") = ", pnorm(x,mean=media,sd=dv,lower.tail = FALSE))
    return(resultado)
  })
  
  output$densnor8<-renderPlot({
    x1<-input$b
    media<-input$mu4
    dv<-input$sigma4
    
    xp <- seq(media-6,6+media,0.01)
    hx <- dnorm(xp,mean=media,sd=dv)
    
    dat<-data.frame(xp,hx)
    
    f<-ggplot(data=dat, mapping = aes(xp,hx))+geom_line()+
      geom_area(mapping = aes(xp,hx), fill = "blue",alpha = 0.2)+
      geom_area(mapping = aes(x=ifelse(xp>=x1,xp,NA),y=ifelse(xp>=x1,dnorm(xp,mean=media,sd=dv),NA)), fill = "blue",alpha = 0.5)+
      geom_segment(aes(x = x1, y =0 , xend = x1,
                       yend = dnorm(x1,mean=media,sd=dv)),
                   colour = "black",linetype=2)+
      labs( title = 'Densidad Normal',
            x = "x", y = "f(x)",caption = "http://synergy.vision/" )+
      scale_x_continuous(limits = c(media-6,media+6))
    return(f)
  })
  
  
  
  
  
  }


shinyApp(ui = ui, server = server)
