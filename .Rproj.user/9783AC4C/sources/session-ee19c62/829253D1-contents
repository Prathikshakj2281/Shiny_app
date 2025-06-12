library(shiny)
library(tidyverse)
library(janitor)

ui <- fluidPage(
  tableOutput("yearwise_table"),
  plotOutput("yearwise_plot1"),
  plotOutput("yearwise_plot2")
  
)

server <- function(input, output, session) {
  data1<-reactive({
    "10_Property_stolen_and_recovered.csv" %>% 
      read_csv() %>% 
      clean_names()
  })
  data2<-reactive({
    data1() %>% 
      rename("state_ut"="area_name")
  })
  
  data3<-reactive({
    data2() %>% 
     filter(group_name !="Total Property")
  })
  
  output$total_cases<-renderText({
    data3() %>% 
      group_by(year)
      
  })
  
}

shinyApp(ui, server)