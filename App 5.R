library(shiny)

ui <- fluidPage(
  # Inputs
  textInput("Id1", label = "Enter Name"),
  textInput("Id2", label = "Enter Place"),
  
  # Output
  textOutput("outId1")
)

server <- function(input, output, session) {
  output$outId1 <- renderText({
    paste0(input$Id1, " is from ", input$Id2)
  }) 
}

shinyApp(ui = ui, server = server)
