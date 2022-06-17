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
# This is a test set, please revert to balance_heatmap.csv after
use_this_for_balance <- read_csv("data/heatmap/balance_heatmap_5.csv")

# Converting timestamps
use_this_for_balance$timestamp <- as.Date(use_this_for_balance$timestamp, format =  "%d/%m/%Y")

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
                                      HTML("<b>Choose Dates</b>"),
                                      dateRangeInput("heatmapDate", "Date range:",
                                                     start = "2022-03-01",
                                                     end   = "2023-05-24", 
                                                     min = "2022-03-01",
                                                     max = "2023-05-24"),
                                      
                                      selectInput("dateDivHeatmap", label = "Division:", 
                                                  choices = list("Daily" = "daily",
                                                                 "Monthly" = "monthly",
                                                                 "Yearly" = "yearly"
                                                                 )),
                                      
                                      HTML("<b>Ranking</b>"),
                                      checkboxInput("rankingHeatMapCheck", label = "Implement ranking", value = FALSE),
                                      
                                      selectInput("ranking1", label = "Choose criteria to rank:", 
                                                  choices = list("Balance" = "balance",
                                                                 "Income" = "income",
                                                                 "All Expense" = "allExpense",
                                                                 "Food Expense" = "foodExpense",
                                                                 "Education Expense" = "educationExpense", 
                                                                 "Shelter Expense" = "shelterExpense",
                                                                 "Recreation Expense" = "recreationExpense",
                                                                 "Rent Adjustment Expense" = "rentAdjustmentExpense",
                                                                 "None" = "none")),
                                      checkboxInput("ranking1Ascending", label = "Descending order", value = FALSE),
                                      
                                      selectInput("ranking2", label = "Choose criteria to rank:", 
                                                  choices = list("None" = "none", 
                                                                 "Income" = "income",
                                                                 "All Expense" = "allExpense",
                                                                 "Balance" = "balance",
                                                                 "Food Expense" = "foodExpense",
                                                                 "Education Expense" = "educationExpense", 
                                                                 "Shelter Expense" = "shelterExpense",
                                                                 "Recreation Expense" = "recreationExpense",
                                                                 "Rent Adjustment Expense" = "rentAdjustmentExpense"
                                                                 )),
                                      checkboxInput("ranking2Ascending", label = "Descending order", value = FALSE),
                                      
                                      selectInput("ranking3", label = "Choose criteria to rank:", 
                                                  choices = list("None" = "none",
                                                                 "Income" = "income",
                                                                 "All Expense" = "allExpense",
                                                                 "Balance" = "balance",
                                                                 "Food Expense" = "foodExpense",
                                                                 "Education Expense" = "educationExpense", 
                                                                 "Shelter Expense" = "shelterExpense",
                                                                 "Recreation Expense" = "recreationExpense",
                                                                 "Rent Adjustment Expense" = "rentAdjustmentExpense"
                                                                 )),
                                      checkboxInput("ranking3Ascending", label = "Descending order", value = FALSE),
                                      
                                      sliderInput("participantDivision", label = "Choose the number of people to view", 
                                                  min = 0, 
                                                  max = 1010, 
                                                  value = c(0, 100))
                                    ),
                                    
                                    mainPanel(
                                      HTML("<h3>HeatMap of Residents in Engagement</h3>"),
                                      plotlyOutput(outputId = "plot3", height = "100%"),
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
    
    df_total <- use_this_for_balance

    df_use_this <- subset(df_total, select = c(participantId, timestamp, balance))
    
    #Filtering based on the timeframe selected
    df <- df_use_this %>% filter(
      between(timestamp, 
              input$heatmapDate[1], 
              input$heatmapDate[2])
    )
    df_total <- df_total %>% filter(
      between(timestamp, 
              input$heatmapDate[1], 
              input$heatmapDate[2])
    )
    
    #Allowing for date division
    if (input$dateDivHeatmap == "daily") {
      df <- df
    }
    
    if (input$dateDivHeatmap == "monthly") {
      
      df$timestamp <- format(as.Date(df$timestamp), "%Y-%m")
      
      df <- df %>%
        group_by(timestamp) %>%
        summarise(across(everything(), list(sum)))
      
      names(df)[names(df) == 'participantId_1'] <- "participantId"
      names(df)[names(df) == 'balance_1'] <- "balance"
    }
    
    if (input$dateDivHeatmap == "yearly") {
      
      df$timestamp <- format(as.Date(df$timestamp), "%Y")
      
      df <- df %>%
        group_by(timestamp) %>%
        summarise(across(everything(), list(sum)))
    }
    
    names(df)[names(df) == 'participantId_1'] <- "participantId"
    names(df)[names(df) == 'balance_1'] <- "balance"
    
    # Implement ranking
    if (input$rankingHeatMapCheck) {
      
      if (input$ranking1 != "none") { 
        
        if (input$ranking1 == "balance") { 
          
          df_stand_in <- subset(df_total, select = c(participantId, balance))
          
          df_stand_in <- df_stand_in %>%
            group_by(participantId) %>%
            summarise(across(everything(), list(sum)))
          
          merged_df <- merge(df, df_stand_in, by="participantId")
          
          #merged_df_ordered <- merged_df[order(merged_df$balance_1, decreasing = input$ranking1Ascending),]
          
          names(merged_df)[names(merged_df) == 'balance_1'] <- "criteria"
          
          df <- merged_df
          
        }
        
        if (input$ranking1 == "income") { 
          
        }
        
        if (input$ranking1 == "allExpense") { 
          
        }
        
        if (input$ranking1 == "foodExpense") { 
          
        }
        
        if (input$ranking1 == "educationExpense") { 
          
        }
        
        if (input$ranking1 == "shelterExpense") { 
          
        }
        
        if (input$ranking1 == "recreationExpense") { 
          
        }
        
        if (input$ranking1 == "rentAdjustmentExpense") { 
          
        }
        
        }
    }
    
    print(df)
    
    df_use_this <- df
    
  })
  
  output$plot3 <- renderPlotly({
    
    timestamp <- dataHeatmap()$timestamp
    participantId <- dataHeatmap()$participantId
    
    print(dataHeatmap())
    
    if("criteria" %in% colnames(dataHeatmap())) {
      p <- ggplot(dataHeatmap(), aes(
        timestamp, 
        reorder(participantId, dataHeatmap()$criteria),
        fill = dataHeatmap()$balance
      )) + 
        geom_tile() +
        #scale_fill_gradientn(colors = hcl.colors(20, "viridis")) +
        scale_fill_gradient(low="white", high="black") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
    
    else {
      p <- ggplot(dataHeatmap(), aes(
        timestamp, 
        participantId,
        fill = dataHeatmap()$balance
      )) + 
        geom_tile() +
        #scale_fill_gradientn(colors = hcl.colors(20, "viridis")) +
        scale_fill_gradient(low="white", high="black") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank())
    }
    
    ggplotly(p)
    
  })
  
  
  }

####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
