library(tidyverse)
library(dplyr)

all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim="\t",guess_max=10000)

all_stats_hits_innings <- all_week_stats


all_week_stats <- all_week_stats %>%
  filter(Stat != "Hits / At Bats", Stat != "IP")%>%
  mutate(stat_value=as.numeric(stat_value)) %>%
  group_by(Stat,week)%>%
  mutate(roto_points=ifelse(Stat =="WHIP"|Stat=="ERA",rank(-stat_value),rank(stat_value)))



hits_ab <- all_stats_hits_innings%>%
  filter(Stat=="Hits / At Bats")

hits_ab <-separate(hits_ab,stat_value,c("hits","at_bats"),sep="/")%>%
  transform(hits=as.numeric(hits),at_bats=as.numeric(at_bats))

batting_avg <- hits_ab%>%
  group_by(real_name)%>%
  summarise(hits=sum(hits),At_bats=sum(at_bats))%>%
  mutate(AVG=hits/At_bats)


Earned_runs <- all_stats_hits_innings%>%
  filter(Stat=="IP"|Stat=="ERA"|Stat=="WHIP")%>%
  transform(stat_value=as.numeric(stat_value))%>%
  spread(Stat,stat_value)

names(Earned_runs)[2] <- "Manager"

Earned_runs <- Earned_runs%>%
  group_by(week)%>%
  mutate(runs=(ERA*IP)/9)%>%
  mutate(walks_hits=IP*WHIP)%>%
  group_by(Manager)%>%
  summarise(Total_runs=sum(runs),Total_walk_hits=sum(walks_hits),IP=sum(IP))


ERA_WHIP <- Earned_runs%>%
  mutate(WHIP=Total_walk_hits/IP,ERA=(Total_runs/IP)*9)%>%
  select(-Total_walk_hits,-Total_runs,-IP)

gathered_ERA_WHIP <- gather(ERA_WHIP,Stat,value,-Manager)%>%
  group_by(Stat)%>%
  mutate(roto=rank(-value))


batting_roto <- all_week_stats%>%
  filter(Stat!="AVG",Stat!="ERA",Stat!="WHIP")
names(batting_roto)[3]<-"Manager"

batting_roto<- batting_roto%>%
  group_by(Manager,Stat)%>%
  summarise(value=sum(stat_value))%>%
  group_by(Stat)%>%
  mutate(roto=rank(value))

final_roto <- rbind(gathered_ERA_WHIP,batting_roto)

roto_season_scoreboard <- final_roto %>%
  group_by(Manager)%>%
  summarise(roto=sum(roto))%>%
  arrange(desc(roto))%>%ungroup()

summed_weekly_roto <- all_week_stats %>%
  group_by(real_name,week) %>%
  summarise(total=sum(roto_points)) %>%
  group_by(real_name)%>%
  summarise(total=sum(total))%>%
  arrange(desc(total))







Scoreboard <- all_week_stats %>% group_by(team_name,week) %>% summarise(total=sum(roto_points))

Scoreboard <- arrange(Scoreboard,week)



winners<- Scoreboard %>% group_by(week)%>%filter(total==max(total))
winner_total <- winners %>% group_by(team_name)%>% summarise(weeks_won = n())%>% arrange(desc(weeks_won))
#plot<-ggplot(summed_weekly_roto,(aes(real_name,total)))


total <- select(summed_weekly_roto,total)
total <- unlist(total)
#plot+geom_col()+coord_cartesian(ylim=(c(1000,1500)))



