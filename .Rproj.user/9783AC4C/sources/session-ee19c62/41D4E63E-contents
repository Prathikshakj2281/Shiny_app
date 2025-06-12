library(shiny)

ui <- fluidPage(
  titlePanel("Sidebar layout"),
  sidebarLayout(
    sidebarPanel(
      h1("T1")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("tab1",
                 column(6, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away."))
        ),
        tabPanel("tab2",
                 h1("tab 2")),
        tabPanel("tab3",
                 column(6, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away.")),
                 column(2, h3("Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R. This lesson will get you started building Shiny apps right away."))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)