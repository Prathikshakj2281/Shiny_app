library(shiny)

ui <- navbarPage("My App",
                 
                 # First tab: Overview
                 tabPanel("Overview",
                          fluidPage(
                            titlePanel("Overview Tab"),
                            fluidRow(
                              column(12,
                                     h4("Welcome to the overview tab."),
                                     p("This is where you can add summary information or introductions.")
                              )
                            )
                          )
                 ),
                 
                 # Second tab: Data
                 tabPanel("Data",
                          fluidPage(
                            titlePanel("Data Table Tab"),
                            fluidRow(
                              column(12,
                                     DT::dataTableOutput("mytable")
                              )
                            )
                          )
                 ),
                 
                 # Third tab: Plot
                 tabPanel("Plot",
                          fluidPage(
                            titlePanel("Plot Tab"),
                            fluidRow(
                              column(12,
                                     plotOutput("myplot")
                              )
                            )
                          )
                 )
)

server <- function(input, output) {
  
  output$mytable <- DT::renderDataTable({
    head(mtcars)
  })
  
  output$myplot <- renderPlot({
    hist(mtcars$mpg, main = "Histogram of MPG", col = "darkgreen", border = "white")
  })
}

shinyApp(ui, server)
