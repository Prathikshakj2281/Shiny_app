library(shiny)

ui <- fluidPage(
  numericInput("num", "Enter a number:", 5),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {
  observe({
    print(paste("Number input changed to:", input$num))
  })
  
  output$result <- renderText({
    paste("You entered:", input$num)
  })
}

shinyApp(ui,server)

