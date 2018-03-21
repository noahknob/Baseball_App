library(shinydashboard)
library(shiny)
library(tidyverse)

all_week_player_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/all_week_player_stats.txt", delim = "\t")
avg_pitcher_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_pitcher_stats.txt", delim = "\t")
avg_batter_stats <- read_delim("/Users/noahknoblauch/Dropbox/Baseball/avg_projected_2018.txt",delim = "\t")

all_week_player_stats <- all_week_player_stats %>%
  mutate(Player = gsub("`|\\'", "", iconv(all_week_player_stats$Player, to = "ASCII//TRANSLIT")))

dashboardPage(
  dashboardHeader(title = "Compile Roster"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Batters", tabName = "batters"),
      menuItem("Pitchers", tabName = "pitchers")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("batters",
              fluidPage(sidebarLayout(
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
                                 , multiple = TRUE, options = list(maxItems = 3)),
                  selectizeInput('UTL', 'UTL', choices = 
                                   (Player = c('',unique(filter(all_week_player_stats, display_position != "SP" , 
                                                                display_position != "RP", 
                                                                display_position != "SP,RP")["Player"])))
                                 , multiple = FALSE)
                ),
                mainPanel(
                  verbatimTextOutput('values_B')
                )
              ), title = 'Options groups for select(ize) input',
              fluidRow(
                DT::dataTableOutput("table_B")
              )
              )),
      tabItem("pitchers",
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
                  verbatimTextOutput('values_P')
                )
              ), title = 'Options groups for select(ize) input',
              fluidRow(
                DT::dataTableOutput("table_P")
              )
              ))
    )
  )
)