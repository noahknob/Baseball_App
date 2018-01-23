library(tidyverse)
library(dplyr)
library(RSQLite)
library(ggplot2)

all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim="\t",guess_max=10000)

all_season_stats <-all_week_stats%>%
  filter(week!=24,week!=23,week!=22,Stat!="Hits / At Bats",Stat!="Innings Pitched")%>%
  mutate(stat_value=as.numeric(stat_value))

distinct(all_season_stats,Stat)

run_value <-seq(from = 16, to = 60, length.out = 20)
RBI_value <- seq(from = 12, to = 74, length.out = 20)
HR_value <-1:20
SB_value <- seq(from = 0, to = 11, length.out = 20)
AVG_value <-seq(from = 0.19, to = 0.365, length.out = 20)
win_value <- seq(from = 0, to = 10, length.out = 20)
save_value <- seq(from = 0, to = 10, length.out = 20)
K_value <- seq(from = 20, to = 120, length.out = 20)
ERA_value <- seq(from = 1.4, to = 8.0, length.out = 20)
WHIP_value <- seq(from = 0.8, to = 1.9, length.out = 20)

some_df <- all_season_stats%>%
  filter(Stat=="Runs")%>%
  summarise(min=min(stat_value),
            max=max(stat_value))


winning_prob <- function(runs,RBI,HR,SB,AVG,wins,saves,Ks,ERA,WHIP){
  percent_win<-all_season_stats%>%
    group_by(week)%>%
    mutate(Run_wins=ifelse(Stat=="Runs",ifelse(runs>stat_value,1,0),0),
           RBI_wins=ifelse(Stat=="Runs Batted In",ifelse(RBI>stat_value,1,0),0),
           HR_wins=ifelse(Stat=="Home Runs",ifelse(HR>stat_value,1,0),0),
           SB_wins=ifelse(Stat=="Stolen Bases",ifelse(SB>stat_value,1,0),0),
           AVG_wins=ifelse(Stat=="AVG",ifelse(AVG>stat_value,1,0),0),
           wins_wins=ifelse(Stat=="Wins",ifelse(wins>stat_value,1,0),0),
           saves_wins=ifelse(Stat=="Saves",ifelse(saves>stat_value,1,0),0),
           Ks_wins=ifelse(Stat=="Strikeouts",ifelse(Ks>stat_value,1,0),0),
           ERA_wins=ifelse(Stat=="ERA",ifelse(ERA<stat_value,1,0),0),
           WHIP_wins=ifelse(Stat=="WHIP",ifelse(WHIP<stat_value,1,0),0))%>%
    ungroup()%>%
    summarise(Runs_prob=sum(Run_wins)/210,
              RBI_prob=sum(RBI_wins)/210,
              HR_prob=sum(HR_wins)/210,
              AVG_prob=sum(AVG_wins)/210,
              SB_prob=sum(SB_wins)/210,
              wins_prob=sum(wins_wins)/210,
              saves_prob=sum(saves_wins)/210,
              Ks_prob=sum(Ks_wins)/210,
              ERA_prob=sum(ERA_wins)/210,
              WHIP_wins=sum(WHIP_wins)/210)
  return(percent_win)
}


prob_list <-list(Runs_df=data_frame(value=0,prob=0),
                 RBI_df=data_frame(value=0,prob=0),
                 HR_df=data_frame(value=0,prob=0),
                 SB_df=data_frame(value=0,prob=0),
                 AVG_df=data_frame(value=0,prob=0),
                 wins_df=data_frame(value=0,prob=0),
                 saves_df=data_frame(value=0,prob=0),
                 Ks_df=data_frame(value=0,prob=0),
                 ERA_df=data_frame(value=0,prob=0),
                 WHIP_df=data_frame(value=0,prob=0))
prob_full_list <- list(Runs_full_df=data_frame(value=0,prob=0),
                       RBI_full_df=data_frame(value=0,prob=0),
                       HR_full_df=data_frame(value=0,prob=0),
                       SB_full_df=data_frame(value=0,prob=0),
                       AVG_full_df=data_frame(value=0,prob=0),
                       wins_full_df=data_frame(value=0,prob=0),
                       saves_full_df=data_frame(value=0,prob=0),
                       Ks_full_df=data_frame(value=0,prob=0),
                       ERA_full_df=data_frame(value=0,prob=0),
                       WHIP_full_df=data_frame(value=0,prob=0))

for (i in 1:20){
  prob_list[["Runs_df"]]<- as.data.frame(prob_list["Runs_df"])%>%
    mutate(value=run_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["Runs_prob"]])
  prob_list[["RBI_df"]]<-as.data.frame(prob_list["Rbi_df"])%>%
    mutate(value=RBI_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["RBI_prob"]])
    
  prob_list[["HR_df"]]<- as.data.frame(prob_list["HR_df"])%>%
    mutate(value=HR_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["HR_prob"]])
  prob_list[["SB_df"]]<-  as.data.frame(prob_list["SB_df"])%>%
    mutate(value=SB_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["SB_prob"]])
  prob_list[["AVG_df"]]<-  as.data.frame(prob_list["AVG_df"])%>%
    mutate(value=AVG_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["AVG_prob"]])
  prob_list[["wins_df"]]<-  as.data.frame(prob_list["wins_df"])%>%
    mutate(value=win_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["wins_prob"]])
  prob_list[["saves_df"]]<-  as.data.frame(prob_list["saves_df"])%>%
    mutate(value=save_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["saves_prob"]])
  prob_list[["Ks_df"]]<-  as.data.frame(prob_list["Ks_df"])%>%
    mutate(value=K_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["Ks_prob"]])
  prob_list[["ERA_df"]]<-  as.data.frame(prob_list["ERA_df"])%>%
    mutate(value=ERA_value[i],
           prob=winning_prob(run_value[i],
                             RBI_value[i],
                             HR_value[i],
                             SB_value[i],
                             AVG_value[i],
                             win_value[i],
                             save_value[i],
                             K_value[i],
                             ERA_value[i],
                             WHIP_value[i])[["ERA_prob"]])
  # prob_list[["WHIP_df"]]<-  as.data.frame(prob_list["WHIP_df"])%>%
  #   mutate(value=WHIP_value[i],
  #          prob=winning_prob(run_value[i],
  #                            RBI_value[i],
  #                            HR_value[i],
  #                            SB_value[i],
  #                            AVG_value[i],
  #                            win_value[i],
  #                            save_value[i],
  #                            K_value[i],
  #                            ERA_value[i],
  #                            WHIP_value[i])[["WHIP_prob"]])
  
  prob_full_list[["Runs_full_df"]] <- rbind(as.data.frame(prob_full_list[["Runs_full_df"]]),as.data.frame(prob_list[["Runs_df"]]))
  prob_full_list[["RBI_full_df"]] <- rbind(as.data.frame(prob_full_list[["RBI_full_df"]]),as.data.frame(prob_list[["RBI_df"]]))
  prob_full_list[["HR_full_df"]] <- rbind(as.data.frame(prob_full_list[["HR_full_df"]]),as.data.frame(prob_list[["HR_df"]]))
  prob_full_list[["SB_full_df"]] <- rbind(as.data.frame(prob_full_list[["SB_full_df"]]),as.data.frame(prob_list[["SB_df"]]))
  prob_full_list[["AVG_full_df"]] <- rbind(as.data.frame(prob_full_list[["AVG_full_df"]]),as.data.frame(prob_list[["AVG_df"]]))
  prob_full_list[["wins_full_df"]] <- rbind(as.data.frame(prob_full_list[["wins_full_df"]]),as.data.frame(prob_list[["wins_df"]]))
  prob_full_list[["saves_full_df"]] <- rbind(as.data.frame(prob_full_list[["saves_full_df"]]),as.data.frame(prob_list[["saves_df"]]))
  prob_full_list[["Ks_full_df"]] <- rbind(as.data.frame(prob_full_list[["Ks_full_df"]]),as.data.frame(prob_list[["Ks_df"]]))
  prob_full_list[["ERA_full_df"]] <- rbind(as.data.frame(prob_full_list[["ERA_full_df"]]),as.data.frame(prob_list[["ERA_df"]]))
 # prob_full_list["WHIP_full_df"] <- rbind(as.data.frame(prob_full_list["Runs_full_df"]),as.data.frame(prob_list["Runs_df"]))
 # prob_full_list["WHIP_full_df"] <- rbind(prob_full_list["WHIP_full_df"],prob_list["WHIP_df"])
}

