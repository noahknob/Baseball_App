library(shiny)
library(ggplot2)
library(readr)



server <- function(input, output, session) {
  all_week_player_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/all_week_player_stats.txt", delim = "\t")
  # Filter data based on selections
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- all_week_player_stats
    limitRoster <- reactive({
      man <- input$Manager
      manager_player_stats <- subset(data, data$Manager == man)
      return(c("All", unique(as.character(manager_player_stats$Player))))
    })
    if (input$Manager != "All") {
      data <- data[data$Manager == input$Manager,]
      
      observe({
        updateSelectInput(session, inputId = "Player",choices = limitRoster(), selected = input$Player)
      })
    
    }
    if (input$week != "All") {
      data <- data[data$week == input$week,]
    }
    if (input$Player != "All") {
      
      data <- data[data$Player == input$Player,]
    }
    data
    
  }))
  
  
  
  
}





