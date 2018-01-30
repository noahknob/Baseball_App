library(tidyverse)
library(dplyr)


all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim="\t",guess_max=10000)

names <- read_delim("/Users/noahknoblauch/Baseball/Team_names.txt",delim="\t")

team_id <- read_delim("/Users/noahknoblauch/Baseball/team_id.txt",delim="\t")


team_id <- inner_join(names,team_id,by="team_name")


all_week_stats <- inner_join(all_week_stats,team_id,by="real_name")

random_id<-sample(10)

new_ids <- team_id


for (i in 1:10){
  new_ids <- team_id%>%
    mutate(team_id=ifelse(team_id==i,random_id[i],new_ids["team_id"]))
}




teams<- 1:10
test<- sample(teams,10,replace=FALSE)




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

team_standings <-list()
matchup<-list(list())
standings<-data_frame()
for (j in 1:10){
  for (i in 1:4){
    matchup[[i]] <- all_week_stats%>%
      filter(team_id==j|team_id==scheduale_list[[j]][[i]],week==i)%>%
      group_by(Stat)%>%
      mutate(wins=ifelse(Stat =="WHIP"|Stat=="ERA",rank(-stat_value)-1,rank(stat_value)-1)) %>%
      group_by(real_name)%>%
      summarise(wins=sum(wins))%>%
      mutate(wins=ifelse(wins%%1==0,wins,wins-0.5))
    standings <- rbind(standings,matchup[[i]])
  }
  team_standings[[j]] <- standings
}


team_standings <-list()
matchup<-data_frame()
standings<-data_frame()
for (j in 1:10){
  for (i in 1:21){
    matchup <- all_week_stats%>%
      filter(team_id==j|team_id==scheduale_list[[j]][[i]],week==i)%>%
      group_by(Stat) %>%
      mutate(wins=ifelse(Stat=="WHIP"|Stat=="ERA",rank(-stat_value)-1,rank(stat_value)-1))%>%
      group_by(real_name) %>%
      summarise(wins=sum(wins))%>%
      mutate(wins=ifelse(wins %% 1==0,wins,wins-0.5))
    standings <- rbind(standings,matchup)
  }
  team_standings[[j]] <- standings
  standings<-data_frame()
}


save <-team_standings




george <- team_standings[[3]]



for (i in 1:10){
  team_standings[i]<-team_stangins%>%
    summarise()
}
Noah_standings <- standings#%>%
  #summarise(win=sum(wins[real_name=="Noah"]),
            losses=sum(wins)-win


TEST<-list(names,Noah_standings,all_week_stats)
