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
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

####################################
# Getting Data                     #
####################################
# Extracting Participant Basic Information 
participantData <- read_csv("data/participant/Participants.csv")

# Extracting Participant Data For Income, Expense and Balance 
incomeExpenseBalanceParticipant <- read_csv("data/participant/comprehensiveParticipantInfoFinal.csv")

# Converting timestamps
incomeExpenseBalanceParticipant$timestamp <- as.Date(incomeExpenseBalanceParticipant$timestamp, format =  "%d/%m/%Y")

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

# Getting data for heatmaps
usethis_balance_without_participant_id_v3 <- read_csv("data/heatmap/usethis_balance_without_participant_id_v3.csv")
usethis_balance_without_participant_id_2_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_2_day_window.csv")
usethis_balance_without_participant_id_3_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_3_day_window.csv")
usethis_balance_without_participant_id_4_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_4_day_window.csv")
usethis_balance_without_participant_id_5_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_5_day_window.csv")
usethis_balance_without_participant_id_6_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_6_day_window.csv")
usethis_balance_without_participant_id_7_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_7_day_window.csv")
usethis_balance_without_participant_id_15_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_15_day_window.csv")
usethis_balance_without_participant_id_25_day_window <- read_csv("data/heatmap/usethis_balance_without_participant_id_25_day_window.csv")

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Income and Expense",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input General Parameters</h3>"),
                                      HTML("<p>This frames the datasets for both individual and population to make the comparison easier</p>"),
                              
                                      HTML("<b>Choose Dates</b>"),
                                      dateRangeInput("date", "Date range:",
                                                     start = "2022-03-01",
                                                     end   = "2023-05-24", 
                                                     min = "2022-03-01",
                                                     max = "2023-05-24"), 
                                      
                                      # We use this to select the date division
                                      HTML("<b>Choose Date Division</b>"),
                                      #checkboxInput("dateDivCheck", label = "Use a different date division", value = FALSE),
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
                                      
                                      # Choose Parameters
                                      HTML("<b>Choose Paramaters</b>"),
                                      checkboxInput("incomeCheck", label = "Evaluate income", value = TRUE),
                                      checkboxInput("allExpenseCheck", label = "Evaluate all expense", value = TRUE),
                                      checkboxInput("educationExpenseCheck", label = "Evaluate education expense", value = FALSE),
                                      checkboxInput("foodExpenseCheck", label = "Evaluate food expense", value = FALSE),
                                      checkboxInput("recreationExpenseCheck", label = "Evaluate recreation expense", value = FALSE),
                                      checkboxInput("shelterExpenseCheck", label = "Evaluate shelter expense", value = FALSE),
                                      checkboxInput("rentAdjustmentExpenseCheck", 
                                                    label = "Evaluate rent adjustment expense", value = FALSE),
                                      checkboxInput("balanceCheck", label = "Evaluate balance", value = TRUE),
                                      
                                      
                                      HTML("<h3>Input Participants Parameters</h3>"),
                                      HTML("<p>Use this to understand one participant from Engagement</p>"),
                                      
                                      numericInput("participant", 
                                                   label = "Participant Number", 
                                                   value = 0),
                                      
                                      # Choose Parameters
                                      HTML("<b>Choose Paramaters</b>"),
                                      checkboxInput("incomeCheckParticipant", label = "Evaluate income", value = TRUE),
                                      checkboxInput("allExpenseCheckParticipant", label = "Evaluate all expense", value = TRUE),
                                      checkboxInput("educationExpenseCheckParticipant", label = "Evaluate education expense", value = FALSE),
                                      checkboxInput("foodExpenseCheckParticipant", label = "Evaluate food expense", value = FALSE),
                                      checkboxInput("recreationExpenseCheckParticipant", label = "Evaluate recreation expense", value = FALSE),
                                      checkboxInput("shelterExpenseCheckParticipant", label = "Evaluate shelter expense", value = FALSE),
                                      checkboxInput("rentAdjustmentExpenseCheckParticipant", 
                                                    label = "Evaluate rent adjustment expense", value = FALSE),
                                      checkboxInput("balanceCheckParticipant", label = "Evaluate balance", value = TRUE),
                                      
                                    ),
                                    
                                    mainPanel(
                                      HTML("<h3>Engagement Statistics</h3>"),
                                      plotlyOutput("plot1"),
                                      
                                      HTML("<h3>Individual Statistics</h3>"),
                                      plotlyOutput("plot2")
                                    )
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel("Patterns with Heatmap", 
                                    sidebarPanel(
                                      # We use this to select the date division
                                      HTML("<b>Choose Date Division</b>"),
                                      #checkboxInput("dateDivCheck", label = "Use a different date division", value = FALSE),
                                      selectInput("divisionHeatmap", label = "Division:", 
                                                  choices = list("1-day" = "1-day", 
                                                                 "2-day" = "2-day", 
                                                                 "3-day" = "3-day", 
                                                                 "4-day" = "4-day", 
                                                                 "5-day" = "5-day", 
                                                                 "6-day" = "6-day", 
                                                                 "7-day" = "7-day", 
                                                                 "15-day" = "15-day", 
                                                                 "25-day" = "25-day", 
                                                                 "Weekdays" = "weekdays", 
                                                                 "Weekends" = "weekends")),
                                      
                                      HTML("<b>Ranking</b>"),
                                      checkboxInput("rankingCheck", label = "Implement ranking", value = FALSE),
                                      
                                      HTML("<b>Choose Participants Ranked</b>"),
                                      sliderInput("participantDivision", label = "Explore Participants Ranked", 
                                                  min = 0, 
                                                  max = 100, 
                                                  value = c(0, 100)), 
                                      
                                      HTML("<b>Clustering</b>"),
                                      checkboxInput("clusteringRowsCheck", label = "Implement row clustering", value = FALSE),
                                      checkboxInput("clusteringColumnsCheck", label = "Implement column clustering", value = FALSE)
                                      
                                    ),
                                    
                                    mainPanel(
                                      ui = fluidPage(
                                        h3("The first heatmap"),
                                        InteractiveComplexHeatmapOutput("heatmap_1")
                                      )
                                    )
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  ####################################
  # Overall                          #
  ####################################
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
    
    #Choosing the type of time division
      
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

  ####################################
  # Participant                      #
  ####################################
  dataParticipant <- reactive({
    
    df <- incomeExpenseBalanceParticipant %>% filter(participantId == input$participant)
    
    # These are the checks for the type of information to add
    if (!input$incomeCheckParticipant) {
      df <- subset(df, select = -c(income))
    }
    
    if (!input$allExpenseCheckParticipant) {
      df <- subset(df, select = -c(allExpense))
    }
    
    if (!input$balanceCheckParticipant) {
      df <- subset(df, select = -c(balance))
    }
    
    if (!input$educationExpenseCheckParticipant) {
      df <- subset(df, select = -c(educationalExpense))
    }
    
    if (!input$foodExpenseCheckParticipant) {
      df <- subset(df, select = -c(foodExpense))
    }
    
    if (!input$recreationExpenseCheckParticipant) {
      df <- subset(df, select = -c(recreationalExpense))
    }
    
    if (!input$shelterExpenseCheckParticipant) {
      df <- subset(df, select = -c(shelterExpense))
    }
    
    if (!input$rentAdjustmentExpenseCheckParticipant) {
      df <- subset(df, select = -c(rentAdjustmentExpense))
    }
    
    #Filtering based on the timeframe selected
    df_use_this <- df %>% filter(
      between(timestamp, 
              input$date[1], 
              input$date[2])
    )

    #Choosing the type of time division
    
    if (input$division != "daily") {
      
      if (input$division != "yearly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y-%m")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
        names(df_use_this)[names(df_use_this) == 'participantId_1'] <- 'participantId'
        
        print(df_use_this)
        
      } 
      
      else if (input$division != "monthly") {
        df_use_this$timestamp <- format(as.Date(df_use_this$timestamp), "%Y")
        
        df_use_this <- df_use_this %>%
          group_by(timestamp) %>%
          summarise(across(everything(), list(sum)))
        
        names(df_use_this)[names(df_use_this) == 'participantId_1'] <- 'participantId'
        
        print(df_use_this)
        
      }
      
    }
    
    df_use_this <- subset(df_use_this, select = -c(participantId))
    
    df_final <- melt(df_use_this, id = c("timestamp"))
    
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(dataParticipant(), 
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
  
  ####################################
  # Heatmap                          #
  ####################################
  
  dataHeatmap <- reactive({
  
    if (!input$divisionHeatmap == "1-day") {
      df <- usethis_balance_without_participant_id_v3
    } 
    
    if (!input$divisionHeatmap == "2-day") {
      df <- usethis_balance_without_participant_id_2_day_window
    } 
    
    if (!input$divisionHeatmap == "3-day") {
      df <- usethis_balance_without_participant_id_3_day_window
    } 
    
    if (!input$divisionHeatmap == "4-day") {
      df <- usethis_balance_without_participant_id_4_day_window
    } 
    
    if (!input$divisionHeatmap == "5-day") {
      df <- usethis_balance_without_participant_id_5_day_window
    } 
    
    if (!input$divisionHeatmap == "6-day") {
      df <- usethis_balance_without_participant_id_6_day_window
    } 
    
    if (!input$divisionHeatmap == "7-day") {
      df <- usethis_balance_without_participant_id_7_day_window
    } 
    
    if (!input$divisionHeatmap == "15-day") {
      df <- usethis_balance_without_participant_id_15_day_window
    } 
    
    if (!input$divisionHeatmap == "25-day") {
      df <- usethis_balance_without_participant_id_25_day_window
    } 
    
    if (!input$divisionHeatmap == "weekdays") {
    } 
    
    if (!input$divisionHeatmap == "weekends") {
    } 
    
  })
  
  ht1 = Heatmap(dataHeatmap(), name = "mat",
                show_row_names = TRUE, show_column_names = FALSE, 
                row_dend_reorder = FALSE, column_title = "no reordering", 
                col = col_fun)
  
  ht1 = draw(ht1)
  
  makeInteractiveComplexHeatmap(input, output, session, ht1, "heatmap_1")
  
  }

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
