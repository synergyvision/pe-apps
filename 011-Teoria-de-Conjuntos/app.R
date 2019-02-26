ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydashboard", "0.7.0")
ensure_version("psych", "1.8.4")
#ensure_version("modeest", "2.1")
ensure_version("matrixStats", "0.54.0")
ensure_version("ggplot2","3.0.0")
ensure_version("sets","1.0-18")
ensure_version("RAM","1.2.1.7")
ensure_version("rje","1.9")
ensure_version("futile.logger","1.4.3")
ensure_version("grid","3.4.3")
ensure_version("VennDiagram","1.6.20")



library(shiny)
library(shinydashboard)
library(readxl)
library(psych)
#library(modeest)
library(matrixStats)
library(ggplot2)
library(sets)
library(RAM)
library(rje)
library(futile.logger)
library(grid)
library(VennDiagram)




ui <- fluidPage(

  titlePanel("Teoría de conjuntos"),

  sidebarLayout(

    sidebarPanel(


      textInput(inputId = "con1", label = "Introducir el primer conjunto",placeholder = "a,b,..."),
      textInput(inputId = "con2", label = "Introducir el segundo conjunto",placeholder = "a,b,..."),

      selectInput(inputId = "ope",label = "Operaciones de cojuntos",choices = c("Unión","Intersección","Diferencia","Complemento","Producto Cartesiano","Potencia"),
                                                         selected = NULL),
      conditionalPanel(condition = "input.ope=='Complemento'",
                       textInput(inputId = "con3",
                                 label = "Introducir Conjunto Universo",
                                 placeholder = "a,b,...")),
      checkboxGroupInput(inputId = "con",label="Selección de Conjuntos",choices = c("Conjunto 1"="c1","Conjunto 2"="c2"),selected=NULL),
      conditionalPanel(condition = "input.ope=='Diferencia'",
                       conditionalPanel(condition = "input.con.indexOf('c1')!=-1 & input.con.indexOf('c2')!=-1",
                       selectInput(inputId = 'dif',
                                   label='Tipo de diferencia',
                                   choices = c('Conjunto 1 - Conjunto 2','Conjunto 2 - Conjunto 1'),
                                   selected = NULL))),
      conditionalPanel(condition = "input.ope=='Producto Cartesiano'",
                       conditionalPanel(condition = "input.con.indexOf('c1')!=-1 & input.con.indexOf('c2')!=-1",
                       selectInput(inputId = 'pc',
                                   label='Elija forma del producto',
                                   choices = c('Conjunto 1 X Conjunto 2','Conjunto 2 X Conjunto 1'),
                                   selected = NULL)))
      ),

    mainPanel(
     tabsetPanel(type = 'tabs',id="c",
      tabPanel('Operación de conjuntos',br(),br(),verbatimTextOutput("op1")),

      tabPanel('Diagrama de Venn',br(),column(width=8,align='center',plotOutput("plot"))))
    )
  )
)



server <- function(input, output,session) {


  conjunto1<-reactive({
    if(is.null(input$con1)){
      return()
    } else{
    as.vector(unlist(strsplit(input$con1,",")))
    }

  })


    conjunto2<-reactive({
     if(is.null(input$con2)){
      return()
     } else{
       as.vector(unlist(strsplit(input$con2,",")))
     }
 })

    conjuntou<-reactive({
      if(is.null(input$con3)){
        return()
      } else{
        as.vector(unlist(strsplit(input$con3,",")))
      }
    })

    d<-reactive({
      w<-rbind(conjunto1(),conjunto2())
      row.names(w)<-c("c1","c2")
      return(w)

    })


 output$op1<-renderPrint({

    if(is.null(input$con)){
      return(print('Introduzca los conjuntos'))
    }
    else{
      if(input$ope=="Unión"){
        if(length(input$con)==1){
          if(input$con=='c1'){
            union(conjunto1(),conjunto1())
          }
          else if(input$con=='c2'){
            union(conjunto2(),conjunto2())
          }
        }
        else{
          union(conjunto1(),conjunto2())
        }
      }
      else if(input$ope=="Intersección"){
        if(length(input$con)==1){
          if(input$con=='c1'){
            intersect(conjunto1(),conjunto1())
          }
          else if(input$con=='c2'){
            intersect(conjunto2(),conjunto2())
          }
        }
        else{
          intersect(conjunto1(),conjunto2())
        }
      }
      else if(input$ope=="Diferencia"){
        if(length(input$con)==1){
          if(input$con=='c1'){
            setdiff(conjunto1(),conjunto1())
          }
          else if(input$con=='c2'){
            setdiff(conjunto2(),conjunto2())
          }
        }
        else{
          if(input$dif=='Conjunto 1 - Conjunto 2'){
          setdiff(conjunto1(),conjunto2())
          }
          else if(input$dif=='Conjunto 2 - Conjunto 1'){
            setdiff(conjunto2(),conjunto1())
          }
        }
      }
      else if(input$ope=="Producto Cartesiano"){
        if(length(input$con)==1){
          if(input$con=='c1'){
            a<-expand.grid(unique(conjunto1()),unique(conjunto1()))
            colnames(a)<-c('Conjunto 1','Conjunto 1')
            return(a)
          }
          else if(input$con=='c2'){
            a<-expand.grid(unique(conjunto2()),unique(conjunto2()))
            colnames(a)<-c('Conjunto 2','Conjunto 2')
            return(a)
          }
        }
        else{
          if(input$pc=='Conjunto 1 X Conjunto 2'){
          a<-expand.grid(unique(conjunto1()),unique(conjunto2()))
          colnames(a)<-c('Conjunto 1','Conjunto 2')
          return(a)
          }
          else if(input$pc=='Conjunto 2 X Conjunto 1'){
            a<-expand.grid(unique(conjunto2()),unique(conjunto1()))
            colnames(a)<-c('Conjunto 2','Conjunto 1')
            return(a)
          }
        }
      }
      else if(input$ope=="Potencia"){
        if(length(input$con)==1){
          if(input$con=='c1'){
            powerSet(unique(conjunto1()))
          }
          else if(input$con=='c2'){
            powerSet(unique(conjunto2()))
        }
      }
        else{
          return('Elija solo un grupo')
        }
    }
    else if(input$ope=="Complemento"){
      if(length(input$con)==1){
        if(input$con=='c1'){
          setdiff(conjuntou(),conjunto1())
        }
        else if(input$con=='c2'){
         setdiff(conjuntou(),conjunto2())
        }
      }
      else{
        return('Elija solo un grupo')
      }
    }
  }
})

output$plot<-renderPlot({
  if(is.null(input$con)){
    return()
  }
  else{
    if(input$ope=="Unión"){
      if(length(input$con)==1){
        if(input$con=='c1'){
          a<-union(conjunto1(),conjunto1())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else if(input$con=='c2'){
          a<-union(conjunto2(),conjunto2())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
      }
      else{
        if(all(conjunto1()%in%conjunto2())==TRUE & all(conjunto2()%in%conjunto1())==FALSE){
          a1<-unique(conjunto1())
          b2<-unique(conjunto2())
          a<-paste(a1,collapse = ' ')
          b<-paste(setdiff(b2,a1),collapse = ' ')
          draw.pairwise.venn(length(b2),length(a1),length(intersect(b2,a1)),fill=c('blue','blue'),cat.pos = 0,category = c(b,a),cat.dist = -0.01)
        }
        else if(all(conjunto2()%in%conjunto1())==TRUE & all(conjunto1()%in%conjunto2())==FALSE){
          a1<-unique(conjunto1())
          b2<-unique(conjunto2())
          a<-paste(setdiff(a1,b2),collapse = ' ')
          b<-paste(b2,collapse = ' ')
          draw.pairwise.venn(length(a1),length(b2),length(intersect(a1,b2)),fill=c('blue','blue'),cat.pos = 0,category = c(a,b),cat.dist = -0.01)
        }
        else if(all(conjunto1()%in%conjunto2())==TRUE & all(conjunto2()%in%conjunto1())==TRUE){
          a<-union(conjunto1(),conjunto2())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else{
        group.venn(list(Conjunto1=conjunto1(), Conjunto2=conjunto2()), label=TRUE,
                   fill = c("blue", "blue"),
                   cat.pos = c(0, 0),
                   lab.cex=1.1,file = NULL,ext = NULL)
        unlink("*.log")
        }
      }
    }
    else if(input$ope=="Intersección"){
      if(length(input$con)==1){
        if(input$con=='c1'){
          a<-intersect(conjunto1(),conjunto1())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else if(input$con=='c2'){
          a<-intersect(conjunto2(),conjunto2())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
      }
      else{
        if(all(conjunto1()%in%conjunto2())==TRUE & all(conjunto2()%in%conjunto1())==FALSE){
          a1<-unique(conjunto1())
          b2<-unique(conjunto2())
          a<-paste(a1,collapse = ' ')
          b<-paste(setdiff(b2,a1),collapse = ' ')
          draw.pairwise.venn(length(b2),length(a1),length(intersect(b2,a1)),fill=c('blue','green'),cat.pos = 0,category = c(b,a),cat.dist = -0.01)
        }
        else if(all(conjunto2()%in%conjunto1())==TRUE & all(conjunto1()%in%conjunto2())==FALSE){
          a1<-unique(conjunto1())
          b2<-unique(conjunto2())
          a<-paste(setdiff(a1,b2),collapse = ' ')
          b<-paste(b2,collapse = ' ')
          draw.pairwise.venn(length(a1),length(b2),length(intersect(a1,b2)),fill=c('blue','green'),cat.pos = 0,category = c(a,b),cat.dist = -0.01)
        }
        else if(all(conjunto1()%in%conjunto2())==TRUE & all(conjunto2()%in%conjunto1())==TRUE){
          a<-union(conjunto1(),conjunto2())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else{
        group.venn(list(Conjunto1=conjunto1(), Conjunto2=conjunto2()), label=TRUE,
                   fill = c("green", "blue"),
                   cat.pos = c(0, 0),
                   lab.cex=1.1,file = NULL,ext = NULL)
        unlink("*.log")
        }
      }
    }
    else if(input$ope=='Diferencia'){
      if(length(input$con)==1){
        if(input$con=='c1'){
          return()
        }
        else if(input$con=='c2'){
          return()
        }
      }
      else{
        if(input$dif=='Conjunto 1 - Conjunto 2'){
        a<-setdiff(conjunto1(),conjunto2())
        a1<-paste(a,collapse = ' ')
        draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else if(input$dif=='Conjunto 2 - Conjunto 1'){
        a<-setdiff(conjunto2(),conjunto1())
        a1<-paste(a,collapse = ' ')
        draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
      }
    }
    else if(input$ope=='Complemento'){
      if(length(input$con)==1){
        if(input$con=='c1'){
          a<-setdiff(conjuntou(),conjunto1())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
        else if(input$con=='c2'){
          a<-setdiff(conjuntou(),conjunto2())
          a1<-paste(a,collapse = ' ')
          draw.single.venn(length(a),fill='blue',category = a1,cat.default.pos = 'text',cat.cex = 1,cat.pos = 180)
        }
      }
      else{
        return()
      }
    }
  }
  # if(is.null(input$con)){
  #   return(NULL)
  # }
  # else if(input$ope=="Unión"){
  #   a<-d()[input$con[1],]
  #   b<-d()[input$con[2],]
  #
  #   group.venn(list(Conjunto1=a, Conjunto2=b), label=TRUE,
  #              fill = c("blue", "blue"),
  #              cat.pos = c(0, 0),
  #              lab.cex=1.1,file = NULL,ext = NULL)
  #
  # } else if(input$ope=="Intersección"){
  #   a1<-d()[input$con[1],]
  #   b1<-d()[input$con[2],]
  #
  #   group.venn(list(Conjunto1=a1, Conjunto2=b1), label=TRUE,
  #              fill = c("green", "blue"),
  #              cat.pos = c(0, 0),
  #              lab.cex=1.1,file = NULL,ext = NULL)
  #
  #
  # } else if(input$ope=="Diferencia"){
  #   a1<-d()[input$con[1],]
  #   b1<-d()[input$con[2],]
  #
  #   group.venn(list(Conjunto1=a1, Conjunto2=b1), label=TRUE,
  #              fill = c("green", "white"),
  #              cat.pos = c(0, 0),
  #              lab.cex=1.1,file = NULL,ext = NULL)
  # }
})

}

shinyApp(ui = ui, server = server)
