library(tidyverse)
library(dplyr)

yahoo_scheduale_func <- function(){
  value <- c(-1:7)
  team.scheduale <- matrix(0 , 10 , 9)
  for (z in 1:9) {
    for (i in 1:9) {
      for (j in 1:9) {
        if (i + value[j] != 0) {
          if (i == j) {
            team.scheduale[i,(i + value[j]) %% 9 ] <- 10
          }
          else team.scheduale[i,(i + value[j]) %% 9 ] <- j
        }
        else next


      }
    }
    team.scheduale[10,z] <- (sum(1:10) - sum(team.scheduale[,z]))
  }
  team.scheduale[,9] <- 10:1
  return(team.scheduale)

}


full_scheduale <- function() {
  rep_sched <- matrix(0,10,21)
  team.scheduale <- yahoo_scheduale_func()
  for (i in 1:10) {
    rep_sched[i,] <- rep(team.scheduale[i,], length.out = 21)
  }
  return(rep_sched)
}

yahoo <- full_scheduale()

sim_standings <- function(num,
                          all_week_stats = read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000),
                          names = read_delim("/Users/noahknoblauch/Baseball/Team_names.txt",delim = "\t"),
                          team_id = read_delim("/Users/noahknoblauch/Baseball/team_id.txt",delim = "\t"),
                          Stats = c("Runs","Runs Batted In","Home Runs","Stolen Bases","AVG","Wins","Saves","Strikeouts","ERA","WHIP"),
                          sim_standings = list(),
                          stat_list = list()) {

  team_id <- inner_join(names,team_id,by = "team_name")

  all_week_stats_n <- all_week_stats %>%
    filter(week != 24,week != 23,week != 22) %>%
    filter(Stat != "Hits / At Bats", Stat != "IP") %>%
    mutate(stat_value = as.numeric(stat_value))

  for (value in 1:num) {
    team_id <- team_id %>%
      mutate(team_id = sample(1:10))

    all_week_stats <- inner_join(all_week_stats_n,team_id,by = "real_name")

    all_team_stats <- split(all_week_stats,all_week_stats$team_id)

    wins_df = data.frame(wins = matrix(0, nrow = 21 , ncol = 1))
    loss_df = data.frame(loss = matrix(0, nrow = 21 , ncol = 1))
    season_standings <- list()

    list_df <- list()
    for (z in 1:10) {
      list_df[[z]] <- split(all_team_stats[[z]],all_team_stats[[z]]$Stat)
    }

    for (j in 1:10) {
      for (i in Stats) {
        for (x in 1:21) {
          if (i == "WHIP" | i == "ERA") {
            if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
              wins_df[[1]][[x]] <- 1
              loss_df[[1]][[x]] <- 0
            }
            if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              wins_df[[1]][[x]] <- 0
              loss_df[[1]][[x]] <- 0
            }
            if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              wins_df[[1]][[x]] <- 0
              loss_df[[1]][[x]] <- 1
            }
          }
          else {
            if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
              wins_df[[1]][[x]] <- 1
              loss_df[[1]][[x]] <- 0
            }
            if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              wins_df[[1]][[x]] <- 0
              loss_df[[1]][[x]] <- 0
            }
            if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
              wins_df[[1]][[x]] <- 0
              loss_df[[1]][[x]] <- 1
            }
          }
        }
        list_df[[j]][[i]] <- mutate(list_df[[j]][[i]],wins = wins_df[["wins"]],losses = loss_df[["loss"]])
      }
      season_standings[[j]] <- bind_rows(list_df[[j]])
      season_standings[[j]] <- season_standings[[j]] %>%
        group_by(real_name) %>%
        summarise(wins = sum(wins),losses = sum(losses),Pct = wins / (wins + losses))
    }
    sim_standings[[value]] <- bind_rows(season_standings)
    sim_standings[[value]] <- sim_standings[[value]] %>%
      arrange(desc(Pct))
  }
  standings_stats <- list(sim_standings,stat_list)

  return(standings_stats)
}

################################################################################################


actual_standings <- function(all_week_stats = read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000),
                             names = read_delim("/Users/noahknoblauch/Baseball/Team_names.txt",delim = "\t"),
                             team_id = read_delim("/Users/noahknoblauch/Baseball/team_id.txt",delim = "\t"),
                             Stats = c("Runs","Runs Batted In","Home Runs","Stolen Bases","AVG","Wins","Saves","Strikeouts","ERA","WHIP"),
                             wins_df = data.frame(wins = matrix(0, nrow = 21 , ncol = 1)),
                             loss_df = data.frame(loss = matrix(0, nrow = 21 , ncol = 1)),
                             season_standings = list(),
                             list_df = list(),
                             stat_list = list()) {

  team_id <- inner_join(names,team_id,by = "team_name")

  all_week_stats <- inner_join(all_week_stats,team_id,by = "real_name")
  all_week_stats <- all_week_stats %>%
    filter(week != 24,week != 23,week != 22) %>%
    filter(Stat != "Hits / At Bats", Stat != "IP") %>%
    mutate(stat_value = as.numeric(stat_value))

  all_team_stats <- split(all_week_stats,all_week_stats$team_id)

  for (z in 1:10) {
    list_df[[z]] <- split(all_team_stats[[z]],all_team_stats[[z]]$Stat)
  }

  for (j in 1:10) {
    for (i in Stats) {
      for (x in 1:21) {
        if (i == "WHIP" | i == "ERA") {
          if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
            wins_df[[1]][[x]] <- 1
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 1
          }
        }
        else {
          if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
            wins_df[[1]][[x]] <- 1
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[yahoo[j,x]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 1
          }
        }
      }
      list_df[[j]][[i]] <- mutate(list_df[[j]][[i]],wins = wins_df[["wins"]],losses = loss_df[["loss"]])
    }
    season_standings[[j]] <- bind_rows(list_df[[j]])
    season_standings[[j]] <- season_standings[[j]] %>%
      group_by(real_name) %>%
      summarise(wins = sum(wins),losses = sum(losses),Pct = wins / (wins + losses))

  }
  real_standings <- bind_rows(season_standings)
  real_standings <- real_standings %>%
    arrange(desc(Pct))

  standings_stats <- list(real_standings,stat_list)

  return(standings_stats)

}



standing_2017 <- actual_standings()[[1]]










############################################ Actual Regular Season Standings 2017 ################################



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
