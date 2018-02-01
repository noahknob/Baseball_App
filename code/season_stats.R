library(tidyverse)


full_stats <- readRDS(file = "~/Desktop/BaseballApp/Baseball_outline/data/full_stats.RDS")

full_stats <- full_stats %>%
  select(-stat_id,-player_key)


stats <- c("Hits", "Batting Average", "Home Runs","Runs Batted In", "Stolen Bases", "Runs",
           "Wins","Saves", "Strikeouts","Earned Run Average","At Bats", "Games Played", "(Walks + Hits)/ Innings Pitched")

league_stats <- full_stats %>%
  filter(stat_name %in% stats, player != "David")

league_stats <- league_stats %>%
  group_by(player) %>%
  distinct()


games_week <- 6

league_stats <- spread(league_stats,stat_name,stat_value)
col_order <- c("player","Games Played","Hits","At Bats","Batting Average", "Runs", "Runs Batted In", "Home Runs","Stolen Bases",
               "Wins","Saves","Strikeouts","Earned Run Average","(Walks + Hits)/ Innings Pitched" )
league_stats <- league_stats[,col_order]
names(league_stats)[2] <- "GP"
names(league_stats)[4] <- "AB"
names(league_stats)[5] <- "AVG"
names(league_stats)[7] <- "RBI"
names(league_stats)[8] <- "HR"
names(league_stats)[9] <- "SB"
names(league_stats)[13] <- "ERA"
names(league_stats)[14] <- "WHIP"


league_stats <- league_stats %>%
  mutate(Runs = as.integer(Runs),
         HR = as.integer(HR),
         RBI = as.integer(RBI),
         GP = as.integer(GP),
         SB = as.integer(SB),
         AVG = as.numeric(AVG),
         Wins = as.integer(Wins),
         Saves = as.integer(Saves),
         Strikeouts = as.integer(Strikeouts),
         ERA = as.numeric(ERA),
         WHIP = as.numeric(WHIP))

Batters <- league_stats %>%
  filter(AB != "NA") %>%
  mutate(Strikeouts = NA)

Pitchers <- league_stats %>%
  filter(ERA != "NA") %>%
  mutate(Runs = NA, HR = NA, Hits = NA,GP = as.numeric(GP))

starting_pitchers <- Pitchers %>%
  filter(Saves <= 1)

matchup_stats <- rbind(Batters,Pitchers) %>%
  select(-AB,-Hits) %>%
  mutate(Runs = as.integer(Runs),
         HR = as.integer(HR),
         RBI = as.integer(RBI),
         GP = as.integer(GP),
         SB = as.integer(SB),
         AVG = as.numeric(AVG),
         Wins = as.integer(Wins),
         Saves = as.integer(Saves),
         Strikeouts = as.integer(Strikeouts),
         ERA = as.numeric(ERA),
         WHIP = as.numeric(WHIP))

matchup_stats <- league_stats %>%
  select(-AB,-Hits) %>%
  mutate(Runs = as.integer(Runs),
         HR = as.integer(HR),
         RBI = as.integer(RBI),
         GP = as.integer(GP),
         SB = as.integer(SB),
         AVG = as.numeric(AVG),
         Wins = as.integer(Wins),
         Saves = as.integer(Saves),
         Strikeouts = as.integer(Strikeouts),
         ERA = as.numeric(ERA),
         WHIP = as.numeric())




avg_week_batter_stats <- Batters %>%
  mutate(avg.GP = (GP/games_week),
         Runs = (Runs)/(avg.GP),
         HR = (HR)/(avg.GP),
         RBI = (RBI)/(avg.GP),
         SB = (SB)/(avg.GP))
saveRDS(avg_week_batter_stats,file = "~/Desktop/BaseballApp/Baseball_outline/data/avg_2017_stats.RDS")
season_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/season_stats.txt",delim = "\t")
team_analysis <- function(player_list, desired_pct,
                          avg_week_batter_stats = readRDS(file = "~/Desktop/BaseballApp/Baseball_outline/data/avg_2017_stats.RDS"),
                          season_stats = read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/season_stats.txt",delim = "\t"),
                          team_mat = matrix(NA,0,6)) {
  team_df <- avg_week_batter_stats %>%
    filter(player %in% player_list) %>%
    select(player,AVG,Runs,RBI,HR,SB) %>% ungroup()

  avg_team_df <- team_df %>%
    summarise(player = NA,
              AVG = mean(as.numeric(AVG)),
              Runs = sum(Runs),
              RBI = sum(RBI),
              HR = sum(HR),
              SB = sum(SB))

  pnorm_df <- avg_team_df %>%
    mutate(player = NA,
           AVG = pnorm(AVG, mean = mean(season_stats[["AVG"]]), sd = sd(season_stats[["AVG"]])),
           Runs = pnorm(Runs, mean = mean(season_stats[["Runs"]]), sd = sd(season_stats[["Runs"]])),
           HR = pnorm(HR, mean = mean(season_stats[["HR"]]), sd = sd(season_stats[["HR"]])),
           RBI = pnorm(RBI, mean = mean(season_stats[["RBI"]]), sd = sd(season_stats[["RBI"]])),
           SB = pnorm(SB, mean = mean(season_stats[["SB"]]), sd = sd(season_stats[["SB"]])))
  team_mat <- rbind(team_mat,
                    as.matrix(team_df),
                    as.matrix(avg_team_df),
                    as.matrix(pnorm_df))
  team_mat <- rbind(team_mat,mean(as.numeric(team_mat[11,]),na.rm = TRUE))
  team_mat <- rbind(team_mat,(as.numeric(team_mat[12,1]) - desired_pct))

  team_mat[10,1] <- "Summed Weekly Avg"
  team_mat[11,1] <- "Pnorm of Stat"
  team_mat[12,1] <- "Winning Pct for all Stats"
  team_mat[13,1] <- "Difference from desired Pct"
  team_mat[12,3:6] <- NA
  team_mat[13,3:6] <- NA


  return(team_mat)

}
test_team <- team_analysis(player_list = c("Buster Posey","Paul Goldschmidt",
                                            "Ian Kinsler", "Evan Longoria",
                                            "Brandon Crawford","Mike Trout",
                                            "Aaron Judge","Bryce Harper","Lucas Duda"), desired_pct = 0.53)

player_list = c("Buster Posey","Paul Goldschmidt",
                "Ian Kinsler", "Evan Longoria",
                "Brandon Crawford","Mike Trout",
                "Aaron Judge","Bryce Harper","Lucas Duda")

