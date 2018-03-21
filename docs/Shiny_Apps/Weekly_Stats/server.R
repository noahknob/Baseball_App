library(shiny)
library(ggplot2)
library(readr)



server <- function(input, output, session) {
  week_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/week_stats.txt", delim = " " )
  # Filter data based on selections
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- week_stats
    if (input$Manager != "All") {
      data <- data[data$Manager == input$Manager,]
    }
    if (input$week != "All") {
      data <- data[data$week == input$week,]
    }
    data
    
  }))
}
