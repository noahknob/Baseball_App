library(tidyverse)
projected_batter_stats <- read_delim("/Users/noahknoblauch/Downloads/FanGraphsLeaderboard.txt",delim = ",",guess_max = 10000)

avg_weekly_projected <- projected_batter_stats %>% 
  group_by(Name) %>% unique() %>% ungroup() %>%
  mutate(week = G/6) %>%
  select(Name, week,"G","HR","R",
         "RBI","SB","AVG") %>%
  mutate(Player_ID = row_number())

test <- gather(avg_weekly_projected, key = "Stat", value,-Name,-week, -Player_ID)

new_test <- test %>%
  group_by(Name) %>%
  mutate(new_value = value/week) %>% ungroup()

avg_test <- new_test %>%
  select(-value) %>%
  spread(Stat, new_value)

avg_projected <- avg_test %>%
  mutate(BA = avg_weekly_projected$AVG, GP = avg_weekly_projected$G) %>%
  select(-AVG, -G)

avg_projected <- avg_projected %>%
  mutate_if(is.double,round,digits = 3)

saveRDS(avg_projected,"/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/avg_projected.RDS")


write_delim(avg_projected,"/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/avg_projected.txt",delim = "\t")
