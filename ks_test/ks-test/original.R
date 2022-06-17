# KS-TEST

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(gapminder)
library(lubridate)
library(dplyr)
library(plotly)
library(ggdist)
library(reshape)
library(bslib)
library(reshape2)

####################################
# Getting Data                     #
####################################
# Extracting Participant Basic Information 
participantData <- read_csv("data/participant/Participants.csv")

# Extracting Participant Data For Income, Expense and Balance 
incomeExpenseBalanceParticipant <- read_csv("data/participant/incomeAndExpenseParticipant.csv")
foodExpenseParticipant <- read_csv("data/participant/foodExpenseParticipant.csv")
educationExpenseParticipant <- read_csv("data/participant/educationExpenseParticipant.csv")
shelterExpenseParticipant <- read_csv("data/participant/shelterExpenseParticipant.csv")
recreationExpenseParticipant <- read_csv("data/participant/educationExpenseParticipant.csv")

# Extracting Engagement-Level Information For Income, Expense and Balance
incomeExpenseBalanceTotal <- read_csv("data/overall/total.csv")
incomeExpenseBalanceMin <- read_csv("data/overall/min.csv")
incomeExpenseBalanceAverage <- read_csv("data/overall/average.csv")
incomeExpenseBalanceMax <- read_csv("data/overall/max.csv")

# Converting timestamps
incomeExpenseBalanceTotal$timestamp <- as.Date(incomeExpenseBalanceTotal$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceMin$timestamp <- as.Date(incomeExpenseBalanceMin$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceAverage$timestamp <- as.Date(incomeExpenseBalanceAverage$timestamp, format =  "%d/%m/%Y")
incomeExpenseBalanceMax$timestamp <- as.Date(incomeExpenseBalanceMax$timestamp, format =  "%d/%m/%Y")

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Income",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<b>Choose Dates</b>"),
                                      dateRangeInput("date", "Date range:",
                                                     start = "2022-03-01",
                                                     end   = "2023-05-24", 
                                                     min = "2022-03-01",
                                                     max = "2023-05-24"), 
                                      
                                      # We use this to select the date division
                                      HTML("<b>Choose Month</b>"),
                                      checkboxInput("monthCheck", label = "Evaluate month", value = FALSE),
                                      selectInput("month", label = "Month:", 
                                                  choices = list("Jan" = "jan", 
                                                                 "Feb" = "feb", 
                                                                 "Mar" = "mar", 
                                                                 "Apr" = "apr",
                                                                 "May" = "may",
                                                                 "Jun" = "jun",
                                                                 "Jul" = "jul",
                                                                 "Aug" = "aug",
                                                                 "Sep" = "sep",
                                                                 "Oct" = "oct",
                                                                 "Nov" = "nov",
                                                                 "Dec" = "dec")),
                                      
                                      # We use this to select the date division
                                      HTML("<b>Choose Date Division</b>"),
                                      checkboxInput("dateDivCheck", label = "Use a different date division", value = FALSE),
                                      selectInput("division", label = "Division:", 
                                                  choices = list("Daily" = "daily", 
                                                                 "Monthly" = "monthly", 
                                                                 "Yearly" = "yearly")),
                                      
                                      HTML("<h3>Input Population Parameters</h3>"),
                                      HTML("<p>Use this to understand the population of Engagement</p>"),
                                      HTML("<p>Note that the ranges are in percentage.</p>"),
                                      HTML("<p>Eg. Setting the income range to 90-100 will give you the top 10% of income-earners 
                                           in Engagement. </p>"),
                                      
                                      # We allow the user to decide whether they'd like to explore Income, Expense, 
                                      # and what type of Expense they will like to explore
                                      
                                      # We use this to select a preliminary level of aggregation
                                      HTML("<b>Choose Aggregation</b>"),
                                      selectInput("aggregation", label = "Aggregation:", 
                                                  choices = list("Maximum" = "maximum", 
                                                                 "Average" = "average", 
                                                                 "Minimum" = "minimum", 
                                                                 "Total" = "total")),
                                      
                                      # Income
                                      HTML("<b>Income</b>"),
                                      
                                      checkboxInput("incomeCheck", label = "Evaluate income", value = TRUE),
                                      
                                      sliderInput("incomeRange", label = "Income Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # All Expense
                                      HTML("<b>All Expense</b>"),
                                      checkboxInput("allExpenseCheck", label = "Evaluate all expense", value = TRUE),
                           
                                      sliderInput("allExpenseRange", label = "All Expense Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # Education Expense
                                      HTML("<b>Education Expense</b>"),
                                      checkboxInput("educationExpenseCheck", label = "Evaluate education expense", value = FALSE),
                                      
                                      #sliderInput("educationExpenseRange", label = "Education Expense Range", 
                                       #           min = 0, 
                                        #          max = 100, 
                                         #         value = c(0, 100)), 
                                      
                                      # Food Expense
                                      HTML("<b>Food Expense</b>"),
                                      checkboxInput("foodExpenseCheck", label = "Evaluate food expense", value = FALSE),
                                      
                                      sliderInput("foodExpenseRange", label = "Food Expense Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # Recreation Expense
                                      HTML("<b>Recreation Expense</b>"),
                                      checkboxInput("recreationExpenseCheck", label = "Evaluate recreation expense", value = FALSE),
                                      
                                      sliderInput("recreationExpenseRange", label = "Recreation Expense Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # Shelter Expense
                                      HTML("<b>Shelter Expense</b>"),
                                      checkboxInput("shelterExpenseCheck", label = "Evaluate shelter expense", value = FALSE),
                                      
                                      sliderInput("shelterExpenseRange", label = "Shelter Expense Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # Rent Adjustment
                                      HTML("<b>Rent Adjustment Expense</b>"),
                                      checkboxInput("rentAdjustmentExpenseCheck", 
                                                    label = "Evaluate rent adjustment expense", value = FALSE),
                                      
                                      sliderInput("rentAdjustmentExpenseRange", label = "Rent Adjustment Expense Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      # Balance
                                      HTML("<b>Balance</b>"),
                                      checkboxInput("balanceCheck", label = "Evaluate balance", value = TRUE),
                                      
                                      sliderInput("balanceRange", label = "All Balance Range", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)),
                                      
                                      
                                      HTML("<h3>Input Participants Parameters</h3>"),
                                      HTML("<p>Use this to understand one participant from Engagement</p>"),
                                      
                                      numericInput("participant", 
                                                   label = "Participant Number", 
                                                   value = 1),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      plotlyOutput("plot1")
                                    )
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel("About", 
                                    titlePanel("About")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  dataOverall <- reactive({
    
    # These are the checks for aggreagation
    if (input$aggregation == "total") {
      df <- incomeExpenseBalanceTotal 
    } 
    
    else if (input$aggregation == "maximum") {
      df <- incomeExpenseBalanceMax 
    } 
    
    else if (input$aggregation == "average") {
      df <- incomeExpenseBalanceAverage 
    } 
    
    else if (input$aggregation == "minimum") {
      df <- incomeExpenseBalanceMin 
    }
    
    
    # These are the checks for the type of information to add
    if (!input$incomeCheck) {
      df <- subset(df, select = -c(income))
    }
    
    if (!input$allExpenseCheck) {
      df <- subset(df, select = -c(allExpense))
    }
    
    if (!input$balanceCheck) {
      df <- subset(df, select = -c(balance))
    }
    
    if (!input$educationExpenseCheck) {
      df <- subset(df, select = -c(educationalExpense))
    }
    
    if (!input$foodExpenseCheck) {
      df <- subset(df, select = -c(foodExpense))
    }
    
    if (!input$recreationExpenseCheck) {
      df <- subset(df, select = -c(recreationalExpense))
    }
    
    if (!input$shelterExpenseCheck) {
      df <- subset(df, select = -c(shelterExpense))
    }
    
    if (!input$rentAdjustmentExpenseCheck) {
      df <- subset(df, select = -c(rentAdjustmentExpense))
    }
    
    #Filtering based on the timeframe selected
    df_use_this <- df %>% filter(
      between(timestamp, 
              input$date[1], 
              input$date[2])
    )
    
    #This is to choose the months
    
    
    #Choosing the type of time division
    if (input$dateDivCheck) {
      
      if (input$division != "daily") {
        
        if (input$division != "yearly") {
          df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y-%m")
          
          df_use_this <- df_use_this %>%
            group_by(timestamp) %>%
            summarise(across(everything(), list(sum)))
          
          print(df_use_this)
          
        } 
        
        else if (input$division != "monthly") {
          df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y")
          
          df_use_this <- df_use_this %>%
            group_by(timestamp) %>%
            summarise(across(everything(), list(sum)))
          
          print(df_use_this)
          
        }
        
      }
      
    }
    
    df_final <- melt(df_use_this, id = c("timestamp"))

  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(dataOverall(), 
                aes(x = timestamp, 
                    y = value, 
                    group = variable, 
                    color = variable)) + 
      geom_line() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank())
    
    ggplotly(p)
  })


  
  }

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
