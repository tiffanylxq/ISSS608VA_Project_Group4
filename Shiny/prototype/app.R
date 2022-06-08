library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)

sales_daily <- read_rds("data/sales_daily.rds")

ui <- navbarPage("Hello World",
  theme = bs_theme(bootswatch = "united", version = 3), 
  tabPanel("Home", "home page content"),
  tabPanel("Business", 
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "Business_type",
                            label = "Category",
                            choices = c("Pubs" = "Recreation (Social Gathering)",
                                        "Restaurants" = "Eating"),
                            selected = "Recreation (Social Gathering)"),
               sliderInput(inputId = "Number",
                           label = "Scenario",
                           min = -10,
                           max = 20,
                           value = 5),
               selectInput(inputId = "Case",
                           label = "Scenario",
                           choices = list("Top 5" = 5,
                                          "Top 10" = 10,
                                          "Bottom 5" = -5)),
               dateRangeInput(inputId = "Date",
                              label = "Select Date Range:",
                              start = "2022-03-01",
                              end = "2023-03-01")),
             mainPanel(
               plotlyOutput("plot1"),
               plotlyOutput("plot2"),
               plotlyOutput("plot3")
             )
           )),
  tabPanel("member 2", "content 2"),
  tabPanel("member 3",  navlistPanel(
    tabPanel("plot 1", "Panel 1 content"),
    tabPanel("plot 2", "Panel 2 content"),
    tabPanel("plot 3", fluidRow(
      selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
      verbatimTextOutput("summary"),
      br(),
      tableOutput("table")
    ))
  ))
)

server <- function (input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
  
  
  data1 <- reactive({
    req(input$Business_type)
    df <- sales_daily %>% filter(purpose %in% input$Business_type) %>%
      filter(date_in >= input$Date[1] & date_in <= input$Date[2]) %>%
      group_by(venueId) %>% summarise(total_sales = sum(daily_sales)) %>%
      top_n(input$Number)
  })
  
  data2 <- reactive({
    req(input$Business_type)
    df <- sales_daily %>% 
      filter(purpose %in% input$Business_type) %>% 
      filter(date_in >= input$Date[1] & date_in < input$Date[2]) %>%
      mutate(YearMonth = format(as.Date(date_in), "%Y-%m")) %>%
      group_by(venueId, YearMonth) %>% 
      summarise(monthly_sales = sum(daily_sales))
  })
  
  data3 <- reactive({
    req(input$Business_type)
    df <- sales_daily %>% 
      filter(purpose %in% input$Business_type) %>%
      filter(date_in >= input$Date[1] & date_in < input$Date[2]) %>%
      mutate(wkday = wday(date_in, label = TRUE, abbr = TRUE)) 
    
    
  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(data1(),
                aes(x=reorder(venueId, - total_sales), y= total_sales, fill=venueId)) +
      geom_bar(stat = "identity")
    ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(data2(),
                aes(x=YearMonth, y= monthly_sales, group=venueId)) +
      geom_line(aes(color=venueId))
    ggplotly(p)
  })
  
  output$plot3 <- renderPlotly({
    # d <- event_data("plotly_click")
    # if(is.null(d)) return(NULL)
    
    p <-  
      ggplot(data3(),aes(x=wkday, y= daily_sales)) +
      geom_boxplot()
    ggplotly(p)
    
    
  })
}

shinyApp(ui, server)