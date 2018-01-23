library(dplyr)

all_week_player_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_player_stats.txt",delim = "\t")


season_stats <- all_week_player_stats%>%
  filter(stat_name!="H/AB",stat_name!="IP",stat_value!="-")%>%
  mutate(stat_value=as.numeric(stat_value))%>%
  group_by(player_name,stat_name)%>%
  summarise(stat_value=sum(stat_value))

Noah_week.1 <- all_week_player_stats%>%
  filter(real_name=="Noah",week==1)

noah.draft <- distinct(Noah_week.1["player_name"])

noah.draft.stats <- season_stats%>%
  filter(player_name %in% noah.draft[[1]])

avg_hr <- noah.draft.stats%>%
  filter(stat_name=="HR")
avg <- sum(avg_hr["stat_value"])
avg/24
