library(tidyverse)
library(dplyr)


simulated_standing <- read_delim("/Users/noahknoblauch/Baseball/simulated_standings_1000.txt",delim = "\t")


avg_standings <- simulated_standing %>%
  group_by(real_name) %>%
  summarise(avg_wins = mean(wins),avg_loss = mean(losses),avg_pct = mean(Pct)) %>%
  arrange(desc(avg_pct))

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
  stat_list <- list()
  for (z in 1:10) {
    list_df[[z]] <- split(all_team_stats[[z]],all_team_stats[[z]]$Stat)
  }
  
  for (j in 1:10) {
    for (i in Stats) {
      for (x in 1:21) { 
        if (i == "WHIP" | i == "ERA") {
          if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
            stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
            wins_df[[1]][[x]] <- 1
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 1
          }
        }
        else { 
          if (list_df[[j]][[i]][["stat_value"]][x] > list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
            stat_list[[i]] <- c(stat_list[[i]],list_df[[j]][[i]][["stat_value"]][x])
            wins_df[[1]][[x]] <- 1
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] == list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
            wins_df[[1]][[x]] <- 0
            loss_df[[1]][[x]] <- 0
          }
          if (list_df[[j]][[i]][["stat_value"]][x] < list_df[[scheduale_list[[j]][[x]]]][[i]][["stat_value"]][x]) {
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


x <- actual_standings()

winning_stats <- x[[2]]

test <- bind_rows(winning_stats)

hist(winning_stats[["AVG"]])

real_df <- read_delim("/Users/noahknoblauch/Baseball/real_standings.txt",delim = "\t")


stat_list <- list()
stat_list[["Home Runs"]][[1]] <- 10


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


y <- sim_standings(500)
stats_1 <- y[[2]]
z <- sim_standings(500)
stats_2 <- z[[2]]

Stats <- c("Runs","Runs Batted In","Home Runs","Stolen Bases","AVG","Wins","Saves","Strikeouts","ERA","WHIP")
winning_stats <- list()

for (i in 1:10) {
  winning_stats[[Stats[i]]] <- c(stats_1[[i]],stats_2[[i]])
}


save(winning_stats, file = "/Users/noahknoblauch/Baseball/winning_stats.RData")
saveRDS(winning_stats, file = "/Users/noahknoblauch/Baseball/winning_stats.RDS")




plot(winning_stats[["AVG"]],pnorm(winning_stats[["AVG"]],mean = mean(winning_stats[["AVG"]]),sd = sd(winning_stats[["AVG"]])))
plot(ecdf(winning_stats[["AVG"]]))
points(winning_stats[["AVG"]],pnorm(winning_stats[["AVG"]],mean = mean(winning_stats[["AVG"]]),sd = sd(winning_stats[["AVG"]])),col = "red")

n.obs <- sapply(winning_stats, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(winning_stats, "[", i = seq.max)

avg_winning_stats <- data.frame(mat)

avg_winning_stats <- avg_winning_stats %>%
  summarise_all(funs(mean),na.rm = TRUE)

