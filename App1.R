library(shiny)

ui <- fluidPage(
  h1("Hello world"),
  h2("Hello world"),
  h3("Hello world"),
  h4("Hello world"),
  p("This is on R shiny pacakges that makes it easy to view 
    If your name is Prathiksha , it appears on the app
    ")
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
