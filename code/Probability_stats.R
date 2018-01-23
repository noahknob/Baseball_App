x <- seq(-4, 4, length = 100)
hx <- dnorm(x)


plot(x, hx, type = "l", lty = 2, xlab = "x value",
     ylab = "Density", main = "Comparison of t Distributions")


all_week_stats <- read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t",guess_max = 10000)

all_season_stats <- all_week_stats %>%
  filter(week != 24,week != 23,week != 22,Stat != "Hits / At Bats",Stat != "Innings Pitched") %>%
  mutate(stat_value = as.numeric(stat_value))

avg <- all_season_stats %>%
  filter(Stat == "AVG")

HR <- all_season_stats %>%
  filter(Stat == "Home Runs")

RBI <- all_season_stats %>%
  filter(Stat == "Runs Batted In")

SB <- all_season_stats %>%
  filter(Stat == "Stolen Bases")

runs <- all_season_stats %>%
  filter(Stat == "Runs")

wins <- all_season_stats %>%
  filter(Stat == "Wins")

saves <- all_season_stats %>%
  filter(Stat == "Saves")

Ks <- all_season_stats %>%
  filter(Stat == "Strikeouts")

ERA <- all_season_stats %>%
  filter(Stat == "ERA")

WHIP <- all_season_stats %>%
  filter(Stat == "WHIP")

avg <- avg[["stat_value"]]
HR <- HR[["stat_value"]]
RBI <- RBI[["stat_value"]]
SB <- SB[["stat_value"]]
runs <- runs[["stat_value"]]
wins <- wins[["stat_value"]]
saves <- saves[["stat_value"]]
Ks <- Ks[["stat_value"]]
ERA <- ERA[["stat_value"]]
WHIP <- WHIP[["stat_value"]]


avg_ordered <- sort(avg)
avg_dens <- pnorm(avg_ordered,mean = mean(avg_ordered),sd = sd(avg_ordered))

plot(avg,pnorm(avg,mean = mean(avg),sd = sd(avg)))
plot(ecdf(avg))
points(avg,pnorm(avg,mean = mean(avg),sd = sd(avg)),col = "red")

plot(HR,pnorm(HR,mean = mean(HR),sd = sd(HR)))
plot(ecdf(HR))
points(HR,pnorm(HR,mean = mean(HR),sd = sd(HR)),col = "red")

plot(RBI,pnorm(RBI,mean = mean(RBI),sd = sd(RBI)))
plot(ecdf(RBI))
points(RBI,pnorm(RBI,mean = mean(RBI),sd = sd(RBI)),col = "red")

plot(SB,pnorm(SB,mean = mean(SB),sd = sd(SB)))
plot(ecdf(SB))
points(SB,pnorm(SB,mean = mean(SB),sd = sd(SB)),col = "red")

plot(runs,pnorm(runs,mean = mean(runs),sd = sd(runs)))
plot(ecdf(runs))
points(runs,pnorm(runs,mean = mean(runs),sd = sd(runs)),col = "red")

plot(wins,pnorm(wins,mean = mean(wins),sd = sd(wins)))
plot(ecdf(wins))
points(wins,pnorm(wins,mean = mean(wins),sd = sd(wins)),col = "red")

plot(Ks,pnorm(Ks,mean = mean(Ks),sd = sd(Ks)))
plot(ecdf(Ks))
points(Ks,pnorm(Ks,mean = mean(Ks),sd = sd(Ks)),col = "red")

plot(saves,pnorm(saves,mean = mean(saves),sd = sd(saves)))
plot(ecdf(saves))
points(saves,pnorm(saves,mean = mean(saves),sd = sd(saves)),col = "red")

plot(ERA,pnorm(ERA,mean = mean(ERA),sd = sd(ERA)))
plot(ecdf(ERA))
points(ERA,pnorm(ERA,mean = mean(ERA),sd = sd(ERA)),col = "red")

plot(WHIP,pnorm(WHIP,mean = mean(WHIP),sd = sd(WHIP)))
plot(ecdf(WHIP))
points(WHIP,pnorm(WHIP,mean = mean(WHIP),sd = sd(WHIP)),col = "red")






ggplot(data_frame(average = HR),aes(x = average)) + stat_density()



pnorm(HR,mean = hr_mean,sd = hr_sd)


