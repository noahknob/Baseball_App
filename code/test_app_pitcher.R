library(shiny)
library(tidyverse)
all_week_player_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/all_week_player_stats.txt", delim = "\t")
avg_pitcher_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_pitcher_stats.txt", delim = "\t")
all_week_player_stats <- all_week_player_stats %>%
  mutate(Player = gsub("`|\\'", "", iconv(all_week_player_stats$Player, to = "ASCII//TRANSLIT")))

fluidPage(sidebarLayout(
  sidebarPanel(
    # use regions as option groups
    selectizeInput('SP', 'SP', choices = 
                     (Player = c(unique(all_week_player_stats[grepl("SP",all_week_player_stats$display_position),]["Player"])))
                   ,selected = NULL, multiple = TRUE),
    
    # use updateSelectizeInput() to generate options later
    selectizeInput('RP', 'RP', choices = 
                     (Player = c(unique(all_week_player_stats[grepl("RP",all_week_player_stats$display_position),]["Player"])))
                   , multiple = TRUE)
  ),
  mainPanel(
    verbatimTextOutput('values')
  )
), title = 'Options groups for select(ize) input',
fluidRow(
  DT::dataTableOutput("table")
)
)


server <- function(input, output, session) {
  avg_pitcher_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_pitcher_stats.txt", delim = "\t")
  avg_pitcher_stats <- avg_pitcher_stats %>%
    mutate_if(is.double,round,digits = 3)
  
  df <- reactive({
    data <- avg_pitcher_stats %>%
      filter(Name %in% input$SP | Name %in% input$RP )
    return(data)
  })
  output$values <- renderPrint({
    summed_df <- df()
    list("Wins" = sum(summed_df$W), "Saves" = sum(summed_df$SV), "Strikeouts" = sum(summed_df$K),
         "ERA" = mean(summed_df$ERA), "WHIP" = mean(summed_df$WHIP))
  })
  output$table <- DT::renderDataTable(DT::datatable({
    df()
  }))
}



shinyApp(ui = ui , server = server)
