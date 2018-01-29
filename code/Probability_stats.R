x <- seq(-4, 4, length = 100)
hx <- dnorm(x)


plot(x, hx, type = "l", lty = 2, xlab = "x value",
     ylab = "Density", main = "Comparison of t Distributions")


all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000)

all_season_stats <- all_week_stats %>%
  filter(week != 24,week != 23,week != 22,Stat != "Hits / At Bats",Stat != "IP") %>%
  mutate(stat_value = as.numeric(stat_value)) %>%
  mutate(Stat = ifelse(Stat != "Home Runs", Stat,"HR")) %>%
  mutate(Stat = ifelse(Stat != "Runs Batted In", Stat,"RBI")) %>%
  mutate(Stat = ifelse(Stat != "Stolen Bases",Stat,"SB"))

season_stats <- all_season_stats %>%
  spread(Stat,stat_value) #%>%
  # #mutate(avgdist = pnorm(AVG,mean = mean(AVG),sd = sd(AVG)),
  #        ERAdist = pnorm(ERA,mean = mean(ERA), sd = sd(ERA)),
  #        WHIPdist = pnorm(WHIP,mean = mean(WHIP),sd = sd(WHIP)),
  #        HRdist = pnorm(HR,mean = mean(HR), sd = sd(HR)),
  #        RBIdist = pnorm(RBI,mean = mean(RBI),sd = sd(RBI)),
  #        SBdist = pnorm(SB,mean = mean(SB), sd = sd(SB)),
  #        winsdist = pnorm(Wins,mean = mean(Wins),sd = sd(Wins)),
  #        runsdist = pnorm(Runs,mean = mean(Runs), sd = sd(Runs)),
  #        Ksdist = pnorm(Strikeouts,mean = mean(Strikeouts),sd = sd(Strikeouts)),
  #        savesdist = pnorm(Saves,mean = mean(Saves), sd = sd(Saves)))


write_delim(all_season_stats,"/Users/noahknoblauch/Baseball/all_season_stats.txt",delim = "\t")

plot(season_stats[["AVG"]],season_stats[["avgdist"]])
plot(ecdf(season_stats[["AVG"]]))
points(season_stats[["AVG"]],pnorm(season_stats[["AVG"]],mean = mean(season_stats[["AVG"]]),sd = sd(season_stats[["AVG"]])),col = "red")


winning_prob <- function(runs,
                         RBI,
                         HR,
                         SB,
                         AVG,
                         wins,
                         saves,
                         Ks,
                         ERA,
                         WHIP,
                         Stats = c("Runs","RBI","HR","SB","AVG","Wins","Saves","Ks","ERA","WHIP"),
                         season_stats = read_delim("/Users/noahknoblauch/Baseball/prob_season_stats.txt",delim = "\t")) {

  winning_df <- data_frame(Stats = Stats,
                           Value = c(runs,RBI,HR,SB,AVG,wins,saves,Ks,ERA,WHIP),
                           winning.prob = c(pnorm(runs, mean = mean(season_stats[["Runs"]]), sd = sd(season_stats[["Runs"]])),
                                            pnorm(RBI, mean = mean(season_stats[["RBI"]]), sd = sd(season_stats[["RBI"]])),
                                            pnorm(HR, mean = mean(season_stats[["HR"]]), sd = sd(season_stats[["HR"]])),
                                            pnorm(SB, mean = mean(season_stats[["SB"]]), sd = sd(season_stats[["SB"]])),
                                            pnorm(AVG, mean = mean(season_stats[["AVG"]]), sd = sd(season_stats[["AVG"]])),
                                            pnorm(wins, mean = mean(season_stats[["Wins"]]), sd = sd(season_stats[["Wins"]])),
                                            pnorm(saves, mean = mean(season_stats[["Saves"]]), sd = sd(season_stats[["Saves"]])),
                                            pnorm(Ks, mean = mean(season_stats[["Strikeouts"]]), sd = sd(season_stats[["Strikeouts"]])),
                                            pnorm(ERA, mean = mean(season_stats[["ERA"]]), sd = sd(season_stats[["ERA"]])),
                                            pnorm(WHIP, mean = mean(season_stats[["WHIP"]]), sd = sd(season_stats[["WHIP"]]))))
  season_winning_prob <- winning_df %>%
    summarise(Win.Percentage = mean(winning.prob))

  winning.list <- list(winning_df,season_winning_prob)
  return(winning.list)


}

probability_winning <- winning_prob(35,40,10,4,0.290,5,5,65,3.12,1.2)

df <- all_season_stats %>%
  group_by(Stat) %>%
  mutate(avg = mean(stat_value),sd = sd(stat_value), prob = pnorm(stat_value, mean = avg, sd = sd)) %>% ungroup() %>%
  group_by(Stat,real_name) %>%
  summarise(team_avg = mean(stat_value),avg = avg[1],sd = sd[1]) %>%
  mutate(prob = pnorm(team_avg, mean = avg , sd = sd)) %>%
  mutate(prob = ifelse(Stat == "WHIP" | Stat == "ERA", 1 - prob,prob)) %>%
  filter(Stat != "IP")

winning_percentage_probability <- df %>%
  select(-avg,-sd) %>% ungroup() %>%
  group_by(real_name) %>% summarise(winning.prob = mean(prob))


value <- 1
test <- new.function("Noah")
#noah_probaility <- winning_prob(33.7,9.8,)
probability_winning[[1]]

Noah <-  all_season_stats %>%
  filter(real_name == "Noah") %>%
  group_by(Stat) %>%
  summarise(avg = mean(stat_value))

Noah[Stat == "Runs"]["avg"]

x <- c("a", "b", "aaaaaaaaaaa")
toString()
toString(x, width = 8)
