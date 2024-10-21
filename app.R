library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(rsconnect)
Sys.setlocale("LC_TIME", "C")

# Load dataset
games_df <- read.csv("https://raw.githubusercontent.com/zombie-007/NFL-games/refs/heads/main/games.csv")

# Convert 'gameDate' to date format and 'gameTimeEastern' to time format
games_df$gameDate <- as.Date(games_df$gameDate, format = "%m/%d/%Y")
games_df$gameTimeMinutes <- hour(hms(games_df$gameTimeEastern)) * 60 + minute(hms(games_df$gameTimeEastern))

# Plot 1 function: Home vs Visitor team final scores (show team names and scores on hover)
create_plot1 <- function(data) {
  p1 <- ggplot(data, aes(x = homeFinalScore, y = visitorFinalScore, 
                         text = paste("Home Team:", homeTeamAbbr, "<br>",
                                      "Visitor Team:", visitorTeamAbbr, "<br>",
                                      "Home Score:", homeFinalScore, "<br>",
                                      "Visitor Score:", visitorFinalScore))) +
    geom_point(color = 'blue', alpha = 0.6, size = 3) +
    labs(title = "Home Team vs Visitor Team Final Scores", x = "Home Final Score", y = "Visitor Final Score") +
    theme_minimal()
  
  # Add team names and scores to the hover tooltip
  ggplotly(p1, tooltip = "text", source = "plot1") %>%
    layout(dragmode = "select")
}

# Plot 2 function: Game time vs Game date
create_plot2 <- function(data) {
  p2 <- ggplot(data, aes(x = gameDate, y = gameTimeMinutes)) +
    geom_point(alpha = 0.6, size = 3, color = "purple") +
    labs(title = "Game Time vs Game Date", x = "Game Date", y = "Game Time Eastern") +
    theme_minimal() +
    scale_y_continuous(breaks = seq(540, 1260, by = 60),
                       labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60))
  
  ggplotly(p2, source = "plot2")
}

# UI for Shiny App
ui <- fluidPage(
  titlePanel("NFL Games Data Visualization"),
  
  sidebarPanel(
    h4("Instructions:"),
    helpText("Adjust Score Ranges: Use the sliders in the left panel to filter games by home and visitor team final scores."),
    helpText("Select Teams: Use the dropdown menu to select one or more teams."),
    helpText("Hover Over Points: Move mouse over points in the 'Home Team vs Visitor Team Final Scores' plot to see the team names and scores for each game."),
    helpText("Brush Selection: Click and drag mouse over an area of the 'Home vs Visitor Scores' plot to select a group of games. The corresponding dates and times of those games will appear in both the 'Game Time vs Game Date' plot and the Data Table."),
    width = 12
  ),
  
  fluidRow(
    column(4, 
           sliderInput("homeFinalScore", "Home Final Score", min = min(games_df$homeFinalScore), 
                       max = max(games_df$homeFinalScore), value = range(games_df$homeFinalScore), step = 1)
    ),
    column(4, 
           sliderInput("visitorFinalScore", "Visitor Final Score", min = min(games_df$visitorFinalScore), 
                       max = max(games_df$visitorFinalScore), value = range(games_df$visitorFinalScore), step = 1)
    ),
    column(4, 
           selectInput("team", "Select Team", choices = unique(c(games_df$homeTeamAbbr, games_df$visitorTeamAbbr)), 
                       selected = NULL, multiple = TRUE)
    )
  ),
  
  fluidRow(
    column(6, plotlyOutput("plot1")),
    column(6, plotlyOutput("plot2"))
  ),
  
  fluidRow(
    column(12, dataTableOutput("table"))
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive value to store filtered data based on user input
  filtered_data <- reactive({
    data <- games_df %>%
      filter(homeFinalScore >= input$homeFinalScore[1],
             homeFinalScore <= input$homeFinalScore[2],
             visitorFinalScore >= input$visitorFinalScore[1],
             visitorFinalScore <= input$visitorFinalScore[2])
    
    if (!is.null(input$team) && length(input$team) > 0) {
      data <- data %>%
        filter(homeTeamAbbr %in% input$team | visitorTeamAbbr %in% input$team)
    }
    
    return(data)
  })
  
  # Reactive value to store selected points from plot1 (Home vs Visitor team final scores)
  selected_data <- reactiveVal(NULL)
  
  # Capture brush event from plot1 and filter plot2 and table accordingly
  observeEvent(event_data("plotly_selected", source = "plot1"), {
    selected <- event_data("plotly_selected", source = "plot1")
    
    if (!is.null(selected)) {
      # Extract the selected home and visitor final scores
      selected_home_scores <- unique(selected$x)
      selected_visitor_scores <- unique(selected$y)
      
      # Filter the data for the corresponding games
      selected_games <- filtered_data() %>%
        filter(homeFinalScore %in% selected_home_scores & visitorFinalScore %in% selected_visitor_scores)
      selected_data(selected_games)
    } else {
      selected_data(NULL)
    }
  })
  
  # Plot 1: Home vs Visitor team final scores
  output$plot1 <- renderPlotly({
    create_plot1(filtered_data())
  })
  
  # Plot 2: Game time vs Game date (show corresponding points based on selection from Plot 1)
  output$plot2 <- renderPlotly({
    data_to_plot <- filtered_data()
    
    if (!is.null(selected_data())) {
      data_to_plot <- selected_data()
    }
    
    create_plot2(data_to_plot)
  })
  
  # Data table output (show corresponding data based on the selection)
  output$table <- renderDataTable({
    if (!is.null(selected_data())) {
      datatable(selected_data()[-1])
    } else {
      datatable(filtered_data()[-1])
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
