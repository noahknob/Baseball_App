library(shiny)
library(ggplot2)
library(readr)

all_week_player_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/all_week_player_stats.txt", delim = "\t")

ui <-  fluidPage(
  
  titlePanel("Team Roster"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Manager",
                       "Manager:",
                       c("All",
                         unique(as.character(all_week_player_stats$Manager))))
    ),
    column(4,
           selectInput("week",
                       "Week:",
                       c("All",
                         unique(as.character(all_week_player_stats$week))))
    ),
    column(4,
           selectInput("Player",
                       "Player:",
                       c("All",
                         unique(as.character(all_week_player_stats$Player))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)
