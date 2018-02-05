library(dplyr)
library(tidyverse)

all_week_player_stats <- read_delim("/Users/noahknoblauch/DropBox/Baseball/all_week_player_stats.txt",delim  =  "\t")


season_stats <- all_week_player_stats %>%
  filter(stat_name != "H/AB",stat_name != "IP",stat_value != "-") %>%
  mutate(stat_value = as.numeric(stat_value)) %>%
  group_by(player_name,stat_name) %>%
  summarise(stat_value = sum(stat_value))

Noah_week.1 <- all_week_player_stats %>%
  filter(real_name == "Noah",week == 1)

noah.draft <- distinct(Noah_week.1["player_name"])

noah.draft.stats <- season_stats %>%
  filter(player_name %in% noah.draft[[1]])

avg_hr <- noah.draft.stats %>%
  filter(stat_name == "HR")
avg <- sum(avg_hr["stat_value"])
avg/24



Ryan <- all_week_stats %>%
  filter(real_name == "Ryan") %>%
  group_by(Stat) %>%
  summarise(avg = mean(stat_value))

Josh <- all_week_stats %>%
  filter(real_name == "Josh") %>%
  group_by(Stat) %>%
  summarise(avg = mean(stat_value))

Noah <- all_week_stats %>%
  filter(real_name == "Noah") %>%
  group_by(Stat) %>%
  summarise(avg = mean(stat_value))

winning_stats <- read_rds("/Users/noahknoblauch/Baseball/winning_stats.RDS")

n.obs <- sapply(winning_stats, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(winning_stats, "[", i = seq.max)

avg_winning_stats <- data.frame(mat)

avg_winning_stats <- avg_winning_stats %>%
  summarise_all(funs(mean),na.rm = TRUE)
