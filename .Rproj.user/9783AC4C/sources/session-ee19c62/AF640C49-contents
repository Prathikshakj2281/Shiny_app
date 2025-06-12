library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Sample Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1: Overview", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Tab 2: Data View", tabName = "tab2", icon = icon("table")),
      menuItem("Tab 3: Plot", tabName = "tab3", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tab1",
              fluidRow(
                box(title = "Welcome", width = 12, status = "primary",
                    "This is the overview tab. Put introductory content here.")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "tab2",
              fluidRow(
                box(title = "Data Table", width = 12, status = "info",
                    DT::dataTableOutput("mytable"))
              )
      ),
      
      # Third tab content
      tabItem(tabName = "tab3",
              fluidRow(
                box(title = "Sample Plot", width = 12, status = "success",
                    plotOutput("myplot"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Sample data for table
  output$mytable <- DT::renderDataTable({
    head(mtcars)
  })
  
  # Sample plot
  output$myplot <- renderPlot({
    hist(mtcars$mpg, main = "Histogram of MPG", col = "steelblue", border = "white")
  })
}

shinyApp(ui, server)
