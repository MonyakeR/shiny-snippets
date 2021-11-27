library(shiny)
library(readr)
library(dplyr)
library(plotly)
library(reactable)
library(lubridate)

ui <- fluidPage(
  title = "Plotly drill down",
  fluidRow(
    column(
      12,
      plotlyOutput("bar"),
      br(),
      uiOutput("back"),
      reactableOutput("table"),
    )
  )
)

server <- function(input, output, session) {
  
  sales <- read_csv("sales_data_sample.csv")
  
  
  sales <- sales %>%
    mutate(ORDERDATE = lubridate::mdy(ORDERDATE)) %>% 
    filter(year(ORDERDATE) == 2005)
  
  # to know which bar was selected
  selected_bar <- reactiveVal(NULL)
  
  # create the plotly plot
  fig <- sales %>% 
    group_by(ORDERDATE) %>% 
    dplyr::summarise(TOTALSALES =sum(SALES)) %>% 
    plot_ly(x = ~ORDERDATE, y = ~TOTALSALES, type = 'bar', source = "A")
  
  output$bar <- renderPlotly({
    fig
  })
  
  # get the date of the selected bar
  observe({
    selected_date <- event_data(event = "plotly_click", source = "A")$x
    selected_bar(selected_date)
  })
  
  # check if a bar has been selected and show the filtered data for that bar
  output$table <- renderReactable({
    if(length(selected_bar())) {
      sales_table <- sales %>% 
        filter(ORDERDATE == selected_bar())
      
      reactable(
        sales_table,
        defaultPageSize = 5
      )
    }
      
  })
  
  # show the back button to hide drill down table
  output$back <- renderUI({
    if (length(selected_bar())) 
      actionButton("clear", "Hide Table", icon("chevron-left"))
  })
  
  # clear the selection
  observeEvent(input$clear,{
    selected_bar(NULL)
  })
}

shinyApp(ui, server)
