library(tidyverse)
library(dplyr)
library(raster)

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


all_week_player_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_player_stats.txt",delim = "\t",guess_max=10000)

names <-read_delim("/Users/noahknoblauch/Baseball/Team_Names.txt",delim = "\t")

team_managers<-names[[("real_name")]]

manager <- list()

for (i in team_managers){
  team<-all_week_player_stats%>%
    filter(real_name==i)
  for (j in 1:21){
    team_df <- team %>%
      filter(week == j)
    manager[[i]][[j]] <- team_df
  }
}


avg_runs <- all_week_player_stats %>%
  filter(stat_name=="Runs")%>%
  filter(stat_value!="-")%>%
  mutate(stat_value=as.integer(stat_value))%>%
  group_by(player_name,stat_name)%>%
  summarise(total=sum(stat_value),avg=mean(stat_value),std=sd(stat_value),CV=cv(stat_value),CVu=cv(stat_value)/mean(stat_value))#%>%
  #group_by(player_name)%>%
  #summarise(max=max(avg))


avg_player_stats <- all_week_player_stats %>%
  filter(stat_name!="H/AB")%>%
  filter(stat_value!="-")%>%
  mutate(stat_value=as.numeric(stat_value))%>%
  group_by(player_name,stat_name)%>%
  summarise(total=sum(stat_value),
            weeks_played=n(),
            avg=mean(stat_value),
            std=sd(stat_value),
            CV=cv(stat_value),
            CVu=cv(stat_value)/mean(stat_value))

avg_runs <- avg_player_stats%>%
  filter(stat_name=="Runs")%>%
  filter(total>=18)

avg_HR <- avg_player_stats%>%
  filter(stat_name=="HR")%>%
  filter(total>3)

avg_SB <- avg_player_stats%>%
  filter(stat_name=="SB")%>%
  filter(total>5)


avg_AVG <- avg_player_stats%>%
  filter(stat_name=="AVG")%>%
  filter(weeks_played>3)

avg_wins <- avg_player_stats%>%
  filter(stat_name=="Wins")%>%
  filter(total>5)

avg_SV <- avg_player_stats%>%
  filter(stat_name=="Saves")%>%
  filter(total>5)

avg_RBI <- avg_player_stats%>%
  filter(stat_name=="RBI")%>%
  filter(total>15)

avg_Ks <- avg_player_stats%>%
  filter(stat_name=="Strikeouts")%>%
  filter(total>25)


######## AVG WHIP and AVG ERA are weird###########
avg_WHIP <- avg_player_stats%>%
  filter(stat_name=="WHIP")%>%
  filter(weeks_played > 3)

avg_ERA <- avg_player_stats%>%
  filter(stat_name=="ERA")%>%
  filter(weeks_played > 3)
#################################################



# test_run <- manager[["Noah"]][[10]]%>%
#   select(player_name,stat_name,stat_value)%>%
#   spread(stat_name,stat_value)


# test_run <- test_run[c("player_name","H/AB","Runs","HR","RBI","SB","AVG","IP","Wins","Saves","Strikeouts","WHIP","ERA")]
# Paul_Goldschmidt_AVG <- all_week_players_stats %>%
#   filter(player_name=="Paul Goldschmidt",stat_name=="Batting Average")
# df<-select(Paul_Goldschmidt_AVG,week,stat_value)


#plot<-ggplot(summed_weekly_roto,(aes(real_name,total)))
#ggplot(Paul_Goldschmidt_AVG, (aes(week,stat_value)))+geom_point()

opp_stats <-c("WHIP","ERA")

ERA_WHIP_Best <- all_week_stats%>%
  filter(Stat=="WHIP" | Stat=="ERA")%>%
  group_by(Stat,week)%>%
  summarise(best=min(stat_value))

avg_best_stat <- all_week_stats%>%
  filter(Stat!="WHIP",Stat!="ERA")%>%
  group_by(Stat,week)%>%
  summarise(best=max(stat_value))#%>%
  #summarise(avg=mean(best))

avg_best_stat <- rbind(avg_best_stat,ERA_WHIP_Best)%>%
  summarise(avg=mean(best))


