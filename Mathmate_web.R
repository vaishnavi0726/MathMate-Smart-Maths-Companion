library(shiny)

ui <- fluidPage(
  titlePanel("MathMate - Website Version"),
  tabsetPanel(
    tabPanel("Calculator",
             numericInput("a","First Number",10),
             numericInput("b","Second Number",5),
             selectInput("op","Operation",c("Add","Subtract","Multiply","Divide")),
             actionButton("calc","Calculate"),
             verbatimTextOutput("calc_out")
    ),
    tabPanel("Vector Lab",
             textInput("vec","Enter numbers (comma separated)","2,4,6,8"),
             verbatimTextOutput("vec_out")
    ),
    tabPanel("Statistics Lab",
             textInput("stats","Enter marks (comma separated)","90,100,89,78,95"),
             verbatimTextOutput("stats_out")
    ),
    tabPanel("Graph Studio",
             textInput("gdata","Graph data (comma separated)","10,20,30,40"),
             selectInput("gtype","Graph Type",c("Bar","Line","Histogram")),
             plotOutput("graph")
    )
  )
)

server <- function(input, output){
  output$calc_out <- renderPrint({
    input$calc
    isolate({
      if(input$op=="Add") input$a + input$b
      else if(input$op=="Subtract") input$a - input$b
      else if(input$op=="Multiply") input$a * input$b
      else input$a / input$b
    })
  })
  
  output$vec_out <- renderPrint({
    v <- as.numeric(strsplit(input$vec,",")[[1]])
    list(Mean=mean(v), Sum=sum(v), Max=max(v), Sorted=sort(v))
  })
  
  output$stats_out <- renderPrint({
    v <- as.numeric(strsplit(input$stats,",")[[1]])
    list(Mean=round(mean(v),2), Median=median(v), SD=round(sd(v),2), Topper=max(v))
  })
  
  output$graph <- renderPlot({
    v <- as.numeric(strsplit(input$gdata,",")[[1]])
    if(input$gtype=="Bar") barplot(v, col="steelblue", main="Bar Chart")
    else if(input$gtype=="Line") plot(v, type="o", col="red", lwd=2, main="Line Chart")
    else hist(v, col="orange", main="Histogram")
  })
}

shinyApp(ui, server)