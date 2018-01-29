library(tidyverse)
library(dplyr)
library(RSQLite)
library(ggplot2)

all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim="\t",guess_max=10000)

all_season_stats <-all_week_stats%>%
  filter(week!=24,week!=23,week!=22,Stat!="Hits / At Bats",Stat!="Innings Pitched")%>%
  mutate(stat_value=as.numeric(stat_value))


stat_l <- split(all_season_stats,all_season_stats$Stat)
myfunc <- function(x){
  ecds <- ecdf(x$stat_value)
  return(mutate(x,prob=ecds(stat_value)) %>% arrange(desc(prob)))
}
resl <- list()
i <- 1
for( i in 1:length(stat_l)){
  temp_df <- stat_l[[i]]
  resl[[i]]<-myfunc(temp_df)
}


teams <- split(all_season_stats,all_season_stats$real_name)

ecdf_stat <- group_by(all_season_stats,Stat)


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



stats_function <- function(df,stat_name="RBI",stat_val=10){
  percent_win <- df %>% #group_by(week) %>%
    filter(Stat==stat_name) %>%
    mutate(stat_wins=as.integer(stat_val>stat_value))%>% #%>% ungroup()
    summarise(stat_prob=mean(stat_wins))
  return(percent_win)
  }

Runs_function <- function(runs){
  percent_win<-all_season_stats%>%
    group_by(week)%>%
    mutate(runs_wins=ifelse(Stat=="Runs",ifelse(runs>stat_value,1,0),0))%>%
    ungroup()%>%
    summarise(runs_prob=sum(runs_wins)/210)

  return(percent_win)
}










HR_function <- function(HR){
  percent_win<-all_season_stats%>%
    group_by(week)%>%
    mutate(HR_wins=ifelse(Stat=="Home Runs",ifelse(HR>stat_value,1,0),0))%>%
    ungroup()%>%
    summarise(HR_prob=sum(HR_wins)/210)

  return(percent_win)
}

AVG_function <- function(AVG){
  percent_win<-all_season_stats%>%
    group_by(week)%>%
    mutate(AVG_wins=ifelse(Stat=="AVG",ifelse(AVG>stat_value,1,0),0))%>%
    ungroup()%>%
    summarise(AVG_prob=sum(AVG_wins)/210)

  return(percent_win)

}

RBI_function <- function(RBI){
  percent_win<-all_season_stats%>%
    group_by(week)%>%
    mutate(RBI_wins=ifelse(Stat=="Runs Batted In",ifelse(RBI>stat_value,1,0),0))%>%
    ungroup()%>%
    summarise(RBI_prob=sum(RBI_wins)/210)

  return(percent_win)

}

runs_values <- seq(from = 16, to = 60,  2)
runs_df <- data.frame(matrix(0, nrow =length(runs_values) , ncol = 2))
runs_df <- runs_df%>%
  mutate(value=0,prob=0)%>%
  select(-X1,-X2)
for (i in 1:length(runs_values)){
  runs_df[["value"]][[i]]<-(runs_values[i])
  runs_df[["prob"]][[i]]<-(Runs_function(runs_values[i])[[1]])

}

ggplot(runs_df,aes(x=value,y=prob))+geom_point()+xlab("Runs")+ylab("Probability of Win")


RBI_values <- seq(from = 11, to = 75,  2)
RBI_df <- data.frame(matrix(0, nrow =length(RBI_values) , ncol = 2))
RBI_df <- RBI_df%>%
  mutate(value=0,prob=0)%>%
  select(-X1,-X2)
for (i in 1:length(RBI_values)){
  RBI_df[["value"]][[i]]<-(RBI_values[i])
  RBI_df[["prob"]][[i]]<-(RBI_function(RBI_values[i])[[1]])

}

ggplot(RBI_df,aes(x=value,y=prob))+geom_point()+xlab("RBI")+ylab("Probability of Win")


AVG_values <- seq(from = 0.190, to = .390, length.out = 20)
AVG_df <- data.frame(matrix(0, nrow =length(AVG_values) , ncol = 2))
AVG_df <- AVG_df%>%
  mutate(AVG=0,prob=0)%>%
  select(-X1,-X2)
for (i in 1:20){
  AVG_df[["AVG"]][[i]]<-(AVG_values[i])
  AVG_df[["prob"]][[i]]<-as.numeric(AVG_function(AVG_values[i])[[1]])

}

ggplot(AVG_df,aes(x=AVG,y=prob))+geom_point()+xlab("Batting AVG")+ylab("Probability of Win")

HR_Values <-2:20
some_list <- length(HR_Values)
df<-data.frame(matrix(0, nrow =length(HR_Values) , ncol = 2))
for (i in 1:some_list){
  df[[1]][[i]]<-as.integer(HR_Values[i])
  df[[2]][[i]]<-as.numeric(HR_function(HR_Values[i])[[1]])

}
df<-mutate(df,value=X1,percentage=X2)%>%
  select(-X1,-X2)

ggplot(df,aes(x=value,y=percentage))+geom_point()+xlab("HR Value")+ylab("Probability of Win")
