library(tidyverse)
library(dplyr)

stat.given.win <- readRDS("/Users/noahknoblauch/Baseball/winning_stats.RDS")
p.stat <- read_delim("/Users/noahknoblauch/Baseball/prob_season_stats.txt",delim = "\t")

HR.prob.stat <- p.stat[["HR"]] 
HR.prob.stat.win <- stat.given.win[["Home Runs"]]

t <- pnorm(16,mean = mean(HR.prob.stat), sd = sd(HR.prob.stat)) - pnorm(14,mean = mean(HR.prob.stat), sd = sd(HR.prob.stat)) 
t1 <- 1 - pnorm(16,mean = mean(HR.prob.stat.win), sd = sd(HR.prob.stat.win)) 

t.win.stat <- t1*0.5 /t

n.obs <- sapply(stat.given.win, length)
seq.max <- seq_len(max(n.obs))
mat <- sapply(stat.given.win, "[", i = seq.max)

stat_win <- data.frame(mat)

stat_win <- stat_win %>%
  mutate(avgdist = dnorm(AVG,mean = mean(AVG),sd = sd(AVG)),
         ERAdist = dnorm(ERA,mean = mean(ERA, na.rm = TRUE), sd = sd(ERA, na.rm = TRUE)),
         WHIPdist = dnorm(WHIP,mean = mean(WHIP, na.rm = TRUE),sd = sd(WHIP, na.rm = TRUE)),
         HRdist = pnorm(Home.Runs,mean = mean(Home.Runs,na.rm = TRUE), sd = sd(Home.Runs,na.rm = TRUE)),
         RBIdist = dnorm(Runs.Batted.In,mean = mean(Runs.Batted.In,na.rm = TRUE),sd = sd(Runs.Batted.In,na.rm = TRUE)),
         SBdist = dnorm(Stolen.Bases,mean = mean(Stolen.Bases,na.rm = TRUE), sd = sd(Stolen.Bases,na.rm = TRUE)),
         winsdist = dnorm(Wins,mean = mean(Wins,na.rm = TRUE),sd = sd(Wins,na.rm = TRUE)),
         runsdist = dnorm(Runs,mean = mean(Runs,na.rm = TRUE), sd = sd(Runs,na.rm = TRUE)),
         Ksdist = dnorm(Strikeouts,mean = mean(Strikeouts,na.rm = TRUE),sd = sd(Strikeouts,na.rm = TRUE)),
         savesdist = dnorm(Saves,mean = mean(Saves,na.rm = TRUE), sd = sd(Saves,na.rm = TRUE)))

AVG <- data_frame(AVG = unique(stat_win[["AVG"]]), avg.stat.win = unique(stat_win[["avgdist"]]))
Runs <- data_frame(RUNS = unique(stat_win[["RUNS"]]), avg.stat.win = unique(stat_win[["RUNS"]]))
RBI <- data_frame(RBI = unique(stat_win[["RBI"]]), avg.stat.win = unique(stat_win[["RBI"]]))
HR <- data_frame(HR = unique(stat_win[["Home.Runs"]]), avg.stat.win = unique(stat_win[["Home.Runs"]]))
AVG <- data_frame(AVG = unique(stat_win[["AVG"]]), avg.stat.win = unique(stat_win[["avgdist"]]))
Runs <- data_frame(AVG = unique(stat_win[["RUNS"]]), avg.stat.win = unique(stat_win[["RUNS"]]))
AVG <- data_frame(AVG = unique(stat_win[["AVG"]]), avg.stat.win = unique(stat_win[["avgdist"]]))
Runs <- data_frame(AVG = unique(stat_win[["RUNS"]]), avg.stat.win = unique(stat_win[["RUNS"]]))
AVG <- data_frame(AVG = unique(stat_win[["AVG"]]), avg.stat.win = unique(stat_win[["avgdist"]]))
Runs <- data_frame(AVG = unique(stat_win[["RUNS"]]), avg.stat.win = unique(stat_win[["RUNS"]]))


HR <- inner_join(data_frame(HR = unique(stat_win[["Home.Runs"]]), p.stat.win = unique(stat_win[["HRdist"]])),
           data_frame(HR = unique(p.stat[["HR"]]), prob.stat = unique(p.stat[["HRdist"]])),by = "HR") 
  
HR <- HR %>%
  mutate(P_win_stat = (p.stat.win*0.5) / prob.stat)


unique1 <- sort(unique(stat_win[["AVG"]]))
unique2 <- sort(unique(p.stat[["AVG"]]))
x <- setdiff(unique2,unique1)

hist(stat.given.win[["AVG"]])

avg_stats <- stat_win[["AVG"]]
avg_stats[avg_stats == 0.215]
length(avg_stats[avg_stats == 60.215])

gr <- stat_win[["Wins"]][stat_win[["Wins"]] == 5]
p_gr <- p.stat[["Wins"]][p.stat[["Wins"]] == 35]

