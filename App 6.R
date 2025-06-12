library(shiny)

ui <- fluidPage(
  # Inputs
  numericInput("num1", "A", value=0),
  numericInput("num2", "B", value=0),
  numericInput("num3", "C", value=0),
  # Output
  textOutput("multiabc")
)

server <- function(input, output, session) {
  
  # Function to calculate the sum
  multiplication_three_numbers <- function(a, b, c) {
    return(a * b *c)
  }
  
  output$multiabc <- renderText({
    total <- sum_three_numbers(input$num1, input$num2, input$num3)
    paste("The multilication of A, B, and C is:", total)
  })
}

shinyApp(ui = ui, server = server)
