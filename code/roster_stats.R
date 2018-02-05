library(shiny)
library(ggplot2)

all_week_player_stats <- read_delim("/Users/noahknoblauch/Dropbox/Baseball/all_week_player_stats.txt", delim = "\t")


test_all_stat <- all_week_player_stats %>%
  spread(stat_name, stat_value )

all_week_player_stats <- test_all_stat[,c("real_name","week","player_name","display_position","Runs","RBI","HR",
                                          "SB","AVG","Wins","Saves","Strikeouts","ERA","WHIP")]
all_week_player_stats <- all_week_player_stats %>%
  arrange(desc(week))
names(all_week_player_stats)[1] <- "Manager"
names(all_week_player_stats)[3] <- "Player"

all_week_player_stats <- all_week_player_stats %>% 
  mutate(Wins = as.numeric(Wins), HR = as.numeric(HR),
         AVG = as.numeric(AVG), SB = as.numeric(SB), Runs = as.numeric(Runs),
         RBI = as.numeric(RBI), ERA = as.numeric(ERA), WHIP = as.numeric(WHIP),
         Strikeouts = as.numeric(Strikeouts), Saves = as.numeric(Saves))

write_delim(all_week_player_stats,"/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/all_week_player_stats.txt", delim = "\t")


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


server <- function(input, output) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- all_week_player_stats
    if (input$Manager != "All") {
      data <- data[data$Manager == input$Manager,]
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


shinyApp(ui = ui, server = server)
