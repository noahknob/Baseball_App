library(shiny)
library(tidyverse)
all_week_player_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/all_week_player_stats.txt", delim = "\t")
avg_batter_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_batter_stats.txt",delim = "\t")
avg_batter_stats <- avg_batter_stats %>%
  mutate(Name = iconv(avg_batter_stats$Name, to = "ASCII//TRANSLIT"))
all_week_player_stats <- all_week_player_stats %>%
  mutate(Player = gsub("`|\\'", "", iconv(all_week_player_stats$Player, to = "ASCII//TRANSLIT")))
  

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    # use regions as option groups
    selectizeInput('Catcher', 'C', choices = 
      (Player = c('',unique(all_week_player_stats[grepl("C",all_week_player_stats$display_position),]["Player"])))
      ,selected = NULL, multiple = FALSE),
    
    # use updateSelectizeInput() to generate options later
    selectizeInput('FirstBase', '1B', choices = 
                     (Player = c('',unique(all_week_player_stats[grepl("1B",all_week_player_stats$display_position),]["Player"])))
                   , multiple = FALSE),
    
    # an ordinary selectize input without option groups
    selectizeInput('SecondBase', '2B', choices = 
                     (Player = c('',unique(all_week_player_stats[grepl("2B",all_week_player_stats$display_position),]["Player"])))
                   , multiple = FALSE),
    # a select input
    selectizeInput('ThirdBase', '3B', choices = 
                     (Player = c('',unique(all_week_player_stats[grepl("3B",all_week_player_stats$display_position),]["Player"])))
                   , multiple = FALSE),
    selectizeInput('Shortstop', 'SS', choices = 
                     (Player = c('',unique(all_week_player_stats[grepl("SS",all_week_player_stats$display_position),]["Player"])))
                   , multiple = FALSE),
    selectizeInput('Outfield', 'OF', choices = 
                     (Player = c(unique(all_week_player_stats[grepl("OF",all_week_player_stats$display_position),]["Player"])))
                   , multiple = TRUE),
    selectizeInput('UTL', 'UTL', choices = 
                     (Player = c('',unique(filter(all_week_player_stats, display_position != "SP" , 
                                               display_position != "RP", 
                                               display_position != "SP,RP")["Player"])))
                   , multiple = FALSE)
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
  avg_batter_stats <- readRDS("/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/avg_batter_stats.RDS")
  avg_batter_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_batter_stats.txt",delim = "\t")
  avg_batter_stats <- avg_batter_stats %>%
    mutate_if(is.double,round,digits = 2)
  
  df <- reactive({
    data <- avg_batter_stats %>%
      filter(Name == input$Catcher | Name == input$FirstBase | Name == input$SecondBase | Name == input$ThirdBase | 
               Name == input$Shortstop | Name == input$UTL | Name %in% input$Outfield )
    return(data)
  })
  output$values <- renderPrint({
    summed_df <- df()
    list("Runs" = sum(summed_df$R), "RBI" = sum(summed_df$RBI), "HR" = sum(summed_df$HR),
         "AVG" = mean(summed_df$BA))
  })
  output$table <- DT::renderDataTable(DT::datatable({
    df()
  }))
}

   

shinyApp(ui = ui , server = server)
