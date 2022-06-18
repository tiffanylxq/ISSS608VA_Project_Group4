library(shiny)
library(bslib)

Change_Staff <- read_rds("data/Change_Staff_B.rds")
Change_Job <- read_rds("data/Change_Job_B.rds")


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
  tabPanel("Employer",  
    titlePanel("Changes in Employment"),
    navlistPanel( 
        widths = c(2,10),
    tabPanel("Map View", fluidRow(
      tags$style(make_css(list('.box', 
                               c('font-size', 'font-family', 'color'), 
                               c('10px', 'arial', 'Orange')))),
      titlePanel("Interactive City Map View"),
      splitLayout(
        selectInput("period", label = "Period", choices = c("Daily", "Weekly", "Weekday", "Monthly")),
        sliderInput(inputId = "employees",
                    label = "Number of Employees",
                    min = 1,
                    max = 26,
                    value = c(1))),
        verbatimTextOutput("selected"),
      tmapOutput("plot1"),
      DT::dataTableOutput(outputId = "aTable")
    )),
    tabPanel("Turnover Rate", fluidRow(
      splitLayout(
        selectInput("change_filter", label = "Filter by", choices = c("--", "Date", "Week", "Month")), 
        selectInput("change_value", label = "Options", choices = c("--"))
      ), 
      splitLayout( 
        plotOutput("ChangeStaff"),
        plotlyOutput("ChangeStaffJ")
      ), 
      splitLayout( 
        plotOutput("ChangeJob"),
        plotlyOutput("ChangeJobJ")
      )
    )),
    tabPanel("Further Information", fluidRow(
      selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
      verbatimTextOutput("summary"),
      br(),
      tableOutput("table")
    ))
  ))
)

server <- function (input, output, session) {
  
  #############   Employer - Map View   #########################
  selected_period <- reactive({
     if (input$period == "Daily") {
       Count_Checkin_Daily
     } else if (input$period == "Weekly") {
       Count_Checkin_Weekly
     } else if (input$period == "Weekday") {
       Count_Checkin_Weekday
     } else {
       Count_Checkin_Monthly
     } %>%
      filter(EMPLOYEES == input$employees) 
  })
  
  output$selected <- renderPrint({
    summary(selected_period()) 
  })
  
  output$plot1 <- renderTmap({
    tmap_mode("view")
    tm_shape(buildings) +
    tm_polygons(col = "grey60", size = 1, border.col = "white",border.lwd = 1) +
    tm_shape(shp = selected_period()) +
    tm_symbols(size = 0.5,col = "EMPLOYEES", style = "cont",border.col = "black",
               border.lwd = 0.5, title.col = "Number of\nEmployees")
  })
  
  output$aTable <- DT::renderDataTable({
    if(input$selected){
      DT::datatable(data = selected_period() ,
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
  }) 
  #############   Employer - Turnover Rate   #########################
  selected_filter <- reactive({
    if (input$change_filter == "Date") {
      unique(Change_Staff$Date)
    } else if (input$change_filter == "Week") {
      unique(Change_Staff$Week_Num)
    } else if (input$change_filter == "Month") {
      unique(Change_Staff$Yr_Month)
    } else {
      c("--")
    }
  })
  
  observeEvent(selected_filter(), {
    updateSelectInput(inputId = "change_value", choices = selected_filter()) 
  })
  
  selected_value <- reactive({
    if (input$change_filter == "Date") {
      filter(Change_Staff, Date == input$change_value)
    } else if (input$change_filter == "Week") {
      filter(Change_Staff, Week_Num == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Staff, Yr_Month == input$change_value)
    } else {
      Change_Staff
    } 
    
  })
  
  output$ChangeStaff <- renderPlot({

    ggplot(selected_value(), aes(x= as.factor(Num_of_Employees), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('Employers with Turnover Staff') +
      xlab("No. of Employees") +
      ylab("No. of\nEmployers") +
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  output$ChangeStaffJ <- renderPlotly({
    p<- ggplot(selected_value(), aes(x = educationLevel, y = Num_of_Employees, fill=joviality)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        ),
        aes(text = paste('Employee: ', selected_value()$participantId,
                         'Employer: ', selected_value()$employerId,
                         'Date of Exit: ', selected_value()$Date))
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      coord_flip() +
      ggtitle(label = "Education Level & Joviality")+
      theme_minimal()+
      theme(plot.title = element_text(size=12, face="bold",hjust = 0.5))+
      theme(axis.title.y= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey'))
    
    ggplotly(p, tooltip = 'text') 
    
  })
  
  ###
  observeEvent(selected_filter(), {
    updateSelectInput(inputId = "change_value", choices = selected_filter()) 
  })
  
  selected_value <- reactive({
    if (input$change_filter == "Date") {
      filter(Change_Job, Date == input$change_value)
    } else if (input$change_filter == "Week") {
      filter(Change_Job, Week_Num == input$change_value)
    } else if (input$change_filter == "Month") {
      filter(Change_Job, Yr_Month == input$change_value)
    } else {
      Change_Job
    } 
    
  })
  
  output$Change_Job <- renderPlot({
    
    ggplot(selected_value(), aes(x= as.factor(Num_of_Employers), fill = haveKids)) +
      geom_bar() +
      facet_wrap(~educationLevel)+
      ggtitle('Employers with Turnover Staff') +
      xlab("No. of Employees") +
      ylab("No. of\nEmployers") +
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            axis.line= element_line(color= 'grey'))
    
  })
  
  output$Change_JobJ <- renderPlotly({
    p<- ggplot(selected_value(), aes(x = educationLevel, y = Num_of_Employers, fill=joviality)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        ),
        aes(text = paste('Employee: ', selected_value()$participantId,
                         'Employer: ', selected_value()$employerId,
                         'Date of Job Change: ', selected_value()$Date))
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      coord_flip() +
      ggtitle(label = "Education Level & Joviality")+
      theme_minimal()+
      theme(plot.title = element_text(size=12, face="bold",hjust = 0.5))+
      theme(axis.title.y= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey'))
    
    ggplotly(p, tooltip = 'text') 
    
  })
  
  
  #############   Employer - Further Information   #########################
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