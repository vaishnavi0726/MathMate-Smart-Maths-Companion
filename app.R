library(shiny)

ui <- fluidPage(
  tags$head(tags$style("body{background:#f5f7ff}.well{background:white}")),
  titlePanel("📚 MathMate - Your Smart Maths Companion"),
  tabsetPanel(id="main",
              tabPanel("1. Calculator",
                       sidebarLayout(
                         sidebarPanel(
                           numericInput("n1","Number 1",10),
                           numericInput("n2","Number 2",5),
                           selectInput("op","Operation",c("+","-","*","/","^","%%")),
                           actionButton("goCalc","Calculate",class="btn-primary")
                         ),
                         mainPanel(h3("Result:"), verbatimTextOutput("outCalc"))
                       )
              ),
              tabPanel("2. Vector Lab",
                       textInput("vecIn","Vector (e.g. 2,4,6,8)","2,4,6,8"),
                       verbatimTextOutput("outVec"),
                       plotOutput("plotVec")
              ),
              tabPanel("3. Matrix Lab",
                       textInput("matIn","Matrix numbers (1 to 9)","1,2,3,4,5,6,7,8,9"),
                       numericInput("matRow","Rows",3), numericInput("matCol","Cols",3),
                       verbatimTextOutput("outMat")
              ),
              tabPanel("4. Statistics Lab",
                       textInput("sIn","Student Marks (comma)","90,100,89,78,95,88"),
                       verbatimTextOutput("outStat")
              ),
              tabPanel("5. Student Table",
                       textInput("sName","Names (comma)","sri,sadhu,gopi"),
                       textInput("sMark","Marks (comma)","90,100,89"),
                       tableOutput("outTable")
              ),
              tabPanel("6. Graph Studio",
                       textInput("gIn","Data","10,20,30,40,25"),
                       selectInput("gType","Type",c("Bar","Line","Histogram","Pie")),
                       plotOutput("outGraph")
              ),
              tabPanel("7. Quiz",
                       h4("What is mean of 2,4,6?"),
                       radioButtons("q1","",c("4","6","3")),
                       actionButton("qSub","Submit"),
                       verbatimTextOutput("outQuiz")
              ),
              tabPanel("8. About",
                       h3("MathMate Final Project"),
                       p("Phase 1-4 Combined"),
                       p("Concepts: vectors, data.frame, cut(), apply(), mean(), shiny"),
                       p("Done by you! Full marks !")
              )
  )
)

server <- function(input, output){
  output$outCalc <- renderPrint({
    input$goCalc
    isolate({
      a<-input$n1; b<-input$n2
      if(input$op=="+") a+b else if(input$op=="-") a-b else if(input$op=="*") a*b
      else if(input$op=="/") a/b else if(input$op=="^") a^b else a%%b
    })
  })
  output$outVec <- renderPrint({
    v<-as.numeric(strsplit(input$vecIn,",")[[1]])
    list(Vector=v, Mean=mean(v), Sum=sum(v), Sorted=sort(v))
  })
  output$plotVec <- renderPlot({ v<-as.numeric(strsplit(input$vecIn,",")[[1]]); plot(v,type="o",col="blue",lwd=2,main="Vector Plot") })
  output$outMat <- renderPrint({
    v<-as.numeric(strsplit(input$matIn,",")[[1]]); m<-matrix(v,nrow=input$matRow,ncol=input$matCol); list(Matrix=m, Transpose=t(m), Determinant=if(nrow(m)==ncol(m)) det(m) else "Not Square")
  })
  output$outStat <- renderPrint({
    v<-as.numeric(strsplit(input$sIn,",")[[1]]); list(Mean=mean(v), Median=median(v), SD=sd(v), Topper=max(v), Grade=ifelse(mean(v)>90,"A",ifelse(mean(v)>75,"B","C")))
  })
  output$outTable <- renderTable({
    names<-strsplit(input$sName,",")[[1]]; marks<-as.numeric(strsplit(input$sMark,",")[[1]])
    data.frame(Name=names, Marks=marks, Grade=cut(marks,breaks=c(0,50,75,90,100),labels=c("D","C","B","A")))
  })
  output$outGraph <- renderPlot({
    v<-as.numeric(strsplit(input$gIn,",")[[1]])
    if(input$gType=="Bar") barplot(v,col=rainbow(length(v)),main="Bar Chart")
    else if(input$gType=="Line") plot(v,type="o",col="red",lwd=3,main="Line Chart")
    else if(input$gType=="Histogram") hist(v,col="orange",main="Histogram")
    else pie(v,main="Pie Chart")
  })
  output$outQuiz <- renderPrint({
    input$qSub
    isolate({ if(input$q1=="4") "Correct! 4 than answer da!" else "Wrong da, (2+4+6)/3 = 4" })
  })
}

shinyApp(ui, server)