library(shiny)
library(ggplot2)
library(readr)

week_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/week_stats.txt", delim = " " )

ui <-  fluidPage(
  
  titlePanel("Weekly Stats"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Manager",
                       "Manager:",
                       c("All",
                         unique(as.character(week_stats$Manager))))
    ),
    column(4,
           selectInput("week",
                       "Week:",
                       c("All",
                         unique(as.character(week_stats$week))))
    )),
    
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)