library(tidyverse)
library(dplyr)


all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000)

names <- read_delim("/Users/noahknoblauch/Baseball/Team_names.txt",delim = "\t")

team_id <- read_delim("/Users/noahknoblauch/Baseball/team_id.txt",delim = "\t")


team_id <- inner_join(names,team_id,by = "team_name")

team_id <- team_id %>%
  select(real_name,team_id)



all_week_stats <- inner_join(all_week_stats,team_id,by = "real_name")
all_week_stats3 <- all_week_stats %>%
  filter(week != 24,week != 23,week != 22) %>%
  filter(Stat != "Hits / At Bats", Stat != "IP") %>%
  mutate(stat_value = as.numeric(stat_value))

all_team_stats <- split(all_week_stats,all_week_stats$real_name)


x1 <- c(2,3,4,5,6,7,8,9,10)
x2 <- c(1,10,3,4,5,6,7,8,9)
x3 <- c(9,1,2,10,4,5,6,7,8)
x4 <- c(8,9,1,2,3,10,5,6,7)
x5 <- c(7,8,9,1,2,3,4,10,6)
x6 <- c(10,7,8,9,1,2,3,4,5)
x7 <- c(5,6,10,8,9,1,2,3,4)
x8 <- c(4,5,6,7,10,9,1,2,3)
x9 <- c(3,4,5,6,7,8,10,1,2)
x10 <- c(6,2,7,3,8,4,9,5,1)
team_1_scheduale <- rep_len(x1 ,21)
team_2_scheduale <- rep_len(x2 ,21)
team_3_scheduale <- rep_len(x3 ,21)
team_4_scheduale <- rep_len(x4 ,21)
team_5_scheduale <- rep_len(x5 ,21)
team_6_scheduale <- rep_len(x6 ,21)
team_7_scheduale <- rep_len(x7 ,21)
team_8_scheduale <- rep_len(x8 ,21)
team_9_scheduale <- rep_len(x9 ,21)
team_10_scheduale <- rep_len(x10 ,21)

scheduale_list <- list(team_1_scheduale,
                    team_2_scheduale,
                    team_3_scheduale,
                    team_4_scheduale,
                    team_5_scheduale,
                    team_6_scheduale,
                    team_7_scheduale,
                    team_8_scheduale,
                    team_9_scheduale,
                    team_10_scheduale)
Stats <- c("Runs","Runs Batted In","Home Runs","Stolen Bases","AVG","Wins","Saves","Strikeouts","ERA","WHIP")


simulate_season <- function(num){

  simulated_standings <- list()
  for (value in 1:num) {
   all_week_stats <- all_week_stats3 %>%
     select(-team_id)
    team_id <- team_id %>%
      mutate(team_id = sample(1:10))

    all_week_stats <- inner_join(all_week_stats,team_id,by = "real_name")
    all_id_stats <- all_week_stats
    all_id_stats <- split(all_id_stats,all_id_stats$team_id)

    list_df <- list()
    for (z in 1:10) {
      list_df[[z]] <- split(all_id_stats[[z]],all_id_stats[[z]]$Stat)
    }


    wins_df = data.frame(wins = matrix(0, nrow = 21 , ncol = 1))
    loss_df = data.frame(loss = matrix(0, nrow = 21 , ncol = 1))
    season_standings <- list()
    for (j in 1:10) {
      # random_id_stats<-
        for (i in Stats) {
          for (x in 1:21) {
            wins_df[[1]][[x]] <- ifelse(i == "WHIP" | i == "ERA",
                                        ifelse(list_df[[j]][[i]][["stat_value"]][x] < list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                               1,0),
                                        ifelse(list_df[[j]][[i]][["stat_value"]][x] > list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                               1,0))

            loss_df[[1]][[x]] <- ifelse(i == "WHIP" | i == "ERA",
                                        ifelse(list_df[[j]][[i]][["stat_value"]][x] <= list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                               0,1),
                                        ifelse(list_df[[j]][[i]][["stat_value"]][x] >= list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                               0,1))
            list_df[[j]][[i]] <- mutate(list_df[[j]][[i]],wins = wins_df[["wins"]],losses = loss_df[["loss"]])
          }

        }
      season_standings[[j]] <- bind_rows(list_df[[j]])
      season_standings[[j]] <- season_standings[[j]] %>%
        group_by(real_name) %>%
        summarise(wins = sum(wins),losses = sum(losses),Pct = wins/(wins + losses))
    }

    standings <- bind_rows(season_standings)
    standings <- standings %>%
      arrange(desc(Pct))

    simulated_standings[[value]] <- standings
  }

  return(simulated_standings)
}


############################################ Actual Regular Season Standings 2017 ################################


actual_standings <- function(){
  x1 <- c(2,3,4,5,6,7,8,9,10)
  x2 <- c(1,10,3,4,5,6,7,8,9)
  x3 <- c(9,1,2,10,4,5,6,7,8)
  x4 <- c(8,9,1,2,3,10,5,6,7)
  x5 <- c(7,8,9,1,2,3,4,10,6)
  x6 <- c(10,7,8,9,1,2,3,4,5)
  x7 <- c(5,6,10,8,9,1,2,3,4)
  x8 <- c(4,5,6,7,10,9,1,2,3)
  x9 <- c(3,4,5,6,7,8,10,1,2)
  x10 <- c(6,2,7,3,8,4,9,5,1)
  team_1_scheduale <- rep_len(x1 ,21)
  team_2_scheduale <- rep_len(x2 ,21)
  team_3_scheduale <- rep_len(x3 ,21)
  team_4_scheduale <- rep_len(x4 ,21)
  team_5_scheduale <- rep_len(x5 ,21)
  team_6_scheduale <- rep_len(x6 ,21)
  team_7_scheduale <- rep_len(x7 ,21)
  team_8_scheduale <- rep_len(x8 ,21)
  team_9_scheduale <- rep_len(x9 ,21)
  team_10_scheduale <- rep_len(x10 ,21)

  scheduale_list <- list(team_1_scheduale,
                         team_2_scheduale,
                         team_3_scheduale,
                         team_4_scheduale,
                         team_5_scheduale,
                         team_6_scheduale,
                         team_7_scheduale,
                         team_8_scheduale,
                         team_9_scheduale,
                         team_10_scheduale)
  all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000)
  names <- read_delim("/Users/noahknoblauch/Baseball/Team_names.txt",delim = "\t")

  team_id <- read_delim("/Users/noahknoblauch/Baseball/team_id.txt",delim = "\t")

  team_id <- inner_join(names,team_id,by = "team_name")

  all_week_stats <- inner_join(all_week_stats,team_id,by = "real_name")
  all_week_stats <- all_week_stats %>%
    filter(week != 24,week != 23,week != 22) %>%
    filter(Stat != "Hits / At Bats", Stat != "IP") %>%
    mutate(stat_value = as.numeric(stat_value))

  all_team_stats <- split(all_week_stats,all_week_stats$team_id)

  Stats <- c("Runs","Runs Batted In","Home Runs","Stolen Bases","AVG","Wins","Saves","Strikeouts","ERA","WHIP")

  wins_df = data.frame(wins = matrix(0, nrow = 21 , ncol = 1))
  loss_df = data.frame(loss = matrix(0, nrow = 21 , ncol = 1))
  season_standings <- list()

  list_df <- list()
  for (z in 1:10) {
    list_df[[z]] <- split(all_team_stats[[z]],all_team_stats[[z]]$Stat)
  }

  for (j in 1:10) {
    # random_id_stats<-
    for (i in Stats) {
      for (x in 1:21) {
        wins_df[[1]][[x]] <- ifelse(i == "WHIP" | i == "ERA",
                                    ifelse(list_df[[j]][[i]][["stat_value"]][x] < list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                           1,0),
                                    ifelse(list_df[[j]][[i]][["stat_value"]][x] > list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                           1,0))

        loss_df[[1]][[x]] <- ifelse(i == "WHIP" | i == "ERA",
                                    ifelse(list_df[[j]][[i]][["stat_value"]][x] <= list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                           0,1),
                                    ifelse(list_df[[j]][[i]][["stat_value"]][x] >= list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x],
                                           0,1))
        list_df[[j]][[i]] <- mutate(list_df[[j]][[i]],wins = wins_df[["wins"]],losses = loss_df[["loss"]])
      }

    }
    season_standings[[j]] <- bind_rows(list_df[[j]])
    season_standings[[j]] <- season_standings[[j]] %>%
      group_by(real_name) %>%
      summarise(wins = sum(wins),losses = sum(losses),Pct = wins/(wins + losses))
  }

  real_standings <- bind_rows(season_standings)
  real_standings <- real_standings %>%
    arrange(desc(Pct))

  return(real_standings)

}

real_standings <- actual_standings()

write_delim(real_standings,"/Users/noahknoblauch/Baseball/real_standings.txt",delim = "\t")
write_delim(real_standings,"/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/real_standings.txt",delim = "\t")


simulated_standings <- simulate_season(100)

simulated_standings_1000 <- simulated_standings(1000)
time_elapsed <- system.time( (simulated_standings_1000 <- simulate_season(1000)))

simulated_standings_1000_df <- bind_rows(simulated_standings_1000)
AVG_simulated_standings <- simulated_standings_1000_df %>%
  group_by(real_name) %>%
  summarise(wins = mean(wins),losses = mean(losses),Pct = mean(wins)/(mean(wins) + mean(losses))) %>%
  arrange(desc(Pct))

write_delim(simulated_standings_1000_df,"/Users/noahknoblauch/Baseball/simulated_standings_1000.txt",delim = "\t")


best <- filter(simulated_standings_1000_df,Pct == max(Pct))


simulated_standings_1000_value <- imap(simulated_standings_1000,function(tdf,index){return(mutate(tdf,simulation = index))})

simulated_standings_1000_value <- bind_rows(simulated_standings_1000_value)
playoff_standings <- simulated_standings_1000_value %>%
  group_by(simulation) %>%
  arrange(desc(Pct)) %>%
  slice(1:6) %>%
  ungroup()

playoff_entry <- playoff_standings %>%
  group_by(simulation) %>%
  arrange(desc(Pct)) %>%
  slice(6) %>%
  ungroup()


avg_playoff_entry <- playoff_entry %>%
  summarise(wins = mean(wins),losses = mean(losses),Pct = mean(Pct))

avg_bye_entry <- playoff_standings %>%
  group_by(simulation) %>%
  arrange(desc(Pct)) %>%
  slice(2) %>% ungroup() %>%
  summarise(wins = mean(wins),losses = mean(losses),Pct = mean(Pct))


avg_season_winner <- playoff_standings %>%
  group_by(simulation) %>%
  arrange(desc(Pct)) %>%
  slice(1) %>% ungroup() %>%
  summarise(wins = mean(wins),losses = mean(losses),Pct = mean(Pct))
