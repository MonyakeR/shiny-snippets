library(shiny)
library(readr)
library(dplyr)
library(plotly)
library(reactable)
library(lubridate)
library(bslib)

ui <- page_fluid(
  title = "Plotly drill down",
  h3("Drill down plot in plotly"),
  card(
    card_header("Click on the bar to drill down on the data"),
    card_body(plotlyOutput("bar"))
  ),
  uiOutput("back"),
  reactableOutput("table")
)

server <- function(input, output, session) {
  # read in the data
  sales_raw <- read_csv("sales_data_sample.csv")
  # wrangle the data
  sales <- sales_raw %>%
    mutate(ORDERDATE = lubridate::mdy(ORDERDATE)) %>% 
    filter(year(ORDERDATE) == 2005)
  
  # reactive to know which bar was selected
  selected_bar <- reactiveVal(NULL)
  
  # create the plotly plot
  output$bar <- renderPlotly({
    sales %>% 
      group_by(ORDERDATE) %>% 
      summarise(TOTALSALES = sum(SALES)) %>% 
      plot_ly(x = ~ ORDERDATE, y = ~ TOTALSALES, type = 'bar', source = "A")
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
