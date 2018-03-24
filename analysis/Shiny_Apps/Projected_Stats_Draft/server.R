library(shinydashboard)
library(shiny)
library(tidyverse)

server <- function(input, output, session) {
  avg_pitcher_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_pitcher_stats.txt", delim = "\t")
  avg_batter_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_projected.txt",delim = "\t")
  season_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/season_stats.txt", delim = "\t")
  avg_pitcher_stats <- avg_pitcher_stats %>%
    mutate_if(is.double,round,digits = 3)
  
  df_p <- reactive({
    data <- avg_pitcher_stats %>%
      filter(Name %in% input$SP | Name %in% input$RP )
    return(data)
  })
  
  df_b <- reactive({
    data <- avg_batter_stats %>%
      filter(Name == input$Catcher | Name == input$FirstBase | Name == input$SecondBase | Name == input$ThirdBase | 
               Name == input$Shortstop | Name == input$UTL | Name %in% input$Outfield )
    return(data)
  })
  df_b_pnorm <- reactive({
    df <- data_frame(R = pnorm(sum(df_b()$R), mean = mean(season_stats$Runs), sd = sd(season_stats$Runs)),
                     RBI = pnorm(sum(df_b()$RBI), mean = mean(season_stats$RBI), sd = sd(season_stats$RBI)),
                     HR = pnorm(sum(df_b()$HR), mean = mean(season_stats$HR), sd = sd(season_stats$HR)),
                     SB = pnorm(sum(df_b()$SB), mean = mean(season_stats$SB), sd = sd(season_stats$SB)),
                     AVG = pnorm(mean(df_b()$BA), mean = mean(season_stats$AVG), sd = sd(season_stats$AVG)))
    return(df)
  })
  df_p_pnorm <- reactive({
    df <- data_frame(Wins = pnorm(sum(df_p()$W), mean = mean(season_stats$Wins), sd = sd(season_stats$Wins)),
                     SV = pnorm(sum(df_p()$SV), mean = mean(season_stats$Saves), sd = sd(season_stats$Saves)),
                     K = pnorm(sum(df_p()$K), mean = mean(season_stats$Strikeouts), sd = sd(season_stats$Strikeouts)),
                     ERA = 1 - pnorm(mean(df_p()$ERA), mean = mean(season_stats$ERA), sd = sd(season_stats$ERA)),
                     WHIP = 1 - pnorm(mean(df_p()$WHIP), mean = mean(season_stats$WHIP), sd = sd(season_stats$WHIP)))
    return(df)
  })
  output$values_B <- renderPrint({
    summed_df_b <- df_b()
    list("Runs" = sum(summed_df_b$R), "Probability of Winning Runs" = df_b_pnorm()$R,
         "RBI" = sum(summed_df_b$RBI),"Probability of Winning RBI" = df_b_pnorm()$RBI,
         "HR" = sum(summed_df_b$HR), "Probability of Winning HR" = df_b_pnorm()$HR,
         "SB" = sum(summed_df_b$SB), "Probability of Winning SB" = df_b_pnorm()$SB,
         "AVG" = mean(summed_df_b$BA), "Probability of Winning AVG" = df_b_pnorm()$AVG)
  })
  output$table_B <- DT::renderDataTable(DT::datatable({
    df_b()
  }))
  output$values_P <- renderPrint({
    summed_df_p <- df_p()
    list("Wins" = sum(summed_df_p$W), "Probability of Winning Wins" = df_p_pnorm()$Wins,
         "Saves" = sum(summed_df_p$SV), "Probability of Winning Saves" = df_p_pnorm()$SV,
         "Strikeouts" = sum(summed_df_p$K), "Probability of Winning Ks" = df_p_pnorm()$K,
         "ERA" = mean(summed_df_p$ERA), "Probability of Winning ERA" = df_p_pnorm()$ERA,
         "WHIP" = mean(summed_df_p$WHIP),"Probability of Winning WHIP" = df_p_pnorm()$WHIP)
  })
  output$table_P <- DT::renderDataTable(DT::datatable({
    df_p()
  }))
}
