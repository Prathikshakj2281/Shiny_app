library(shiny)
library(shiny)

ui <- fluidPage(
  selectInput("species","Select Species", choices = c("setosa","versicolor","virginica")),
  plotOutput("speciesplot"),
  tableOutput("speciestable")
)

server <- function(input, output, session) {
  
  speciesdata<-reactive({
    iris %>% 
      filter(Species==input$species)
  })
  
  output$speciesplot<-renderPlot({
    speciesdata()%>% 
      ggplot(aes(x=Sepal.Length, y=Sepal.Width))+
      geom_point()+
      labs(title=input$species)
  })

  output$speciestable<-renderTable({
  speciesdata()
  })
  }
shinyApp(ui=ui, server=server)
