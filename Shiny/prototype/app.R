library(shiny)
library(bslib)

ui <- navbarPage("Hello World",
  theme = bs_theme(bootswatch = "united", version = 3), 
  tabPanel("Home", "home page content"),
  tabPanel("member 1", "content 1"),
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
}

shinyApp(ui, server)