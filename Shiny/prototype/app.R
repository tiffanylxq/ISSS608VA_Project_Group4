library(shiny)
library(bslib)


buildings <- read_sf("data/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

employers <- read_sf("data/Employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")

Count_Checkin_Daily <- read_sf("data/Count_Checkin_Daily.csv", 
                               options = "GEOM_POSSIBLE_NAMES=location")    
Count_Checkin_Weekly <- read_sf("data/Count_Checkin_Weekly.csv", 
                                options = "GEOM_POSSIBLE_NAMES=location")  
Count_Checkin_Monthly <- read_sf("data/Count_Checkin_Monthly.csv", 
                                 options = "GEOM_POSSIBLE_NAMES=location")  
Count_Checkin_Weekday <- read_sf("data/Count_Checkin_Weekday.csv", 
                                 options = "GEOM_POSSIBLE_NAMES=location") 

Count_Checkin_Daily$Num_of_Employees <- as.numeric(Count_Checkin_Daily$Num_of_Employees)
Count_Checkin_Weekly$Num_of_Employees <- as.numeric(Count_Checkin_Weekly$Num_of_Employees)
Count_Checkin_Weekday$Num_of_Employees <- as.numeric(Count_Checkin_Weekday$Num_of_Employees)
Count_Checkin_Monthly$Num_of_Employees <- as.numeric(Count_Checkin_Monthly$Num_of_Employees)

ui <- navbarPage("Hello World",
  theme = bs_theme(bootswatch = "united", version = 3), 
  tabPanel("Home", "home page content"),
  tabPanel("member 1", "content 1"),
  tabPanel("member 2", "content 2"),
  tabPanel("Employer",  navlistPanel(
    tabPanel("Map View", fluidRow(
      selectInput("period", label = "Period", choices = c("Daily", "Weekly", "Weekday", "Monthly")),
      verbatimTextOutput("selected"),
      plotOutput("plot1")
    )),
    tabPanel("Turnover Rate", "Panel 2 content"),
    tabPanel("Further Information", fluidRow(
      selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
      verbatimTextOutput("summary"),
      br(),
      tableOutput("table")
    ))
  ))
)

server <- function (input, output, session) {
  selected_period <- reactive({
     if (input$period == "Daily") {
       Count_Checkin_Daily
     } else if (input$period == "Weekly") {
       Count_Checkin_Weekly
     } else if (input$period == "Weekday") {
       Count_Checkin_Weekday
     } else {
       Count_Checkin_Monthly
     }
  })
  
  output$selected <- renderPrint({
    summary(selected_period())
  })
  
  output$plot1 <- renderPlot({
    tmap_mode("view")
    tm_shape(buildings) +
    tm_polygons(col = "grey60", size = 1, border.col = "white",border.lwd = 1) +
    tm_shape(selected_period()) +
    tm_symbols(size = 0.5, col = "Num_of_Employees", style = "cont", title.col = "Number of\nEmployees")
  })
  
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}

shinyApp(ui, server)