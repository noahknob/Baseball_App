library(Lahman)
library(tidyverse)

df <- Batting %>%
  filter(yearID == 2016)
library(baseballr)
library(lubridate)

week_1 <- seq(as.Date("2017-04-02"),as.Date("2017-04-09"),by = "day")

Pre_all_star <- seq(as.Date("2017-04-10"),as.Date("2017-07-09"),by = "day")
long_week <- seq(as.Date("2017-07-14"),as.Date("2017-07-23"),by = "day")
Post_all_star <- seq(as.Date("2017-07-24"),as.Date("2017-10-01"),by = "day")

Dates <- Post_all_star

Pull_week_pitcher_stats <- function(Dates) {

  Dates <- as.data.frame(Dates)
  Mondays <- c(Dates[(format(Dates, "%w") == "1")])
  Sundays <- ceiling_date(as.Date(Mondays), unit = "week")
  week_pitcher_stats <- data_frame()
  for (i in 1:10) {
    stats_df <- daily_pitcher_bref(Mondays[i],Sundays[i])
    stats_df <- stats_df %>%
      mutate(Week = 14 + i)
    week_pitcher_stats <- bind_rows(week_pitcher_stats,stats_df)
  }
return(week_pitcher_stats)
}

Pre_all_star_stats <- Pull_week_pitcher_stats(Dates = Pre_all_star)
long_week_pitcher_stats <- daily_pitcher_bref("2017-07-14", "2017-07-23")
Post_all_star_stats <- Pull_week_pitcher_stats(Dates = Post_all_star)

week_1_pitcher_stats <-  daily_pitcher_bref("2017-04-02", "2017-04-09")

week_1_pitcher_stats <- week_1_pitcher_stats %>%
  mutate(Week = 1)

Pre_all_star_stats <- Pre_all_star_stats %>%
  mutate(Week = Week + 1)

long_week_pitcher_stats <- long_week_pitcher_stats %>%
  mutate(Week = 15)

Post_all_star_stats <- Post_all_star_stats %>%
  mutate(Week = Week + 15)



all_week_pitcher_stats <- bind_rows(week_1_pitcher_stats,Pre_all_star_stats,long_week_pitcher_stats,Post_all_star_stats)

saveRDS(all_week_pitcher_stats,"/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/all_week_pitcher_stats.RDS")





Pull_week_batter_stats <- function(Dates) {

  Dates <- as.data.frame(Dates)
  Mondays <- c(Dates[(format(Dates, "%w") == "1")])
  Sundays <- ceiling_date(as.Date(Mondays), unit = "week")
  week_batter_stats <- data_frame()
  for (i in 1:length(Mondays)) {
    stats_df <- daily_batter_bref(Mondays[i],Sundays[i])
    stats_df <- stats_df %>%
      mutate(Week = (14 + i))
    week_batter_stats <- bind_rows(week_batter_stats,stats_df)
  }
  return(week_batter_stats)
}

week_1_batter_stats <-  daily_batter_bref("2017-04-02", "2017-04-09")
Pre_all_star_batter_stats <- Pull_week_batter_stats(Dates = Pre_all_star)
long_week_batter_stats <- daily_batter_bref("2017-07-14", "2017-07-23")
Post_all_star_batter_stats <- Pull_week_batter_stats(Dates = Post_all_star)


week_1_batter_stats <- week_1_batter_stats %>%
  mutate(Week = 1)

long_week_batter_stats <- long_week_batter_stats %>%
  mutate(Week = 15)

Post_all_star_batter_stats <- Post_all_star_batter_stats %>%
  mutate(Week = Week + 1)


all_week_batter_stats <- bind_rows(week_1_batter_stats,Pre_all_star_batter_stats,long_week_batter_stats,Post_all_star_batter_stats)

saveRDS(all_week_batter_stats,"/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/all_week_batter_stats.RDS")

Paul_Gold <- all_week_batter_stats %>%
  filter(Name == "Paul Goldschmidt") %>%
  summarise(G = sum(G) , PA = sum(PA), AB = sum(AB), H = sum(H) , R = sum(R), HR = sum(HR) , SB = sum(SB), RBI = sum(RBI))


avg_batter_stats <- all_week_batter_stats %>%
  group_by(Name) %>%
  summarise(R = mean(R), RBI = mean(RBI), HR = mean(HR),
            SB = mean(SB), BA = sum(H)/sum(AB), GP = sum(G), Weeks = length(Week))


all_week_pitcher_stats[is.na(all_week_pitcher_stats)] <- 0
avg_pitcher_stats <- all_week_pitcher_stats %>%
  group_by(Name) %>%
  summarise(W = mean(W, na.rm = FALSE), SV = mean(SV, na.rm = FALSE), K = mean(SO),
            ERA = 9*(sum(ER)/sum(IP)), WHIP = mean(WHIP), IP = sum(IP), Weeks = length(Week))

saveRDS(avg_pitcher_stats,"/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/avg_pitcher_stats.RDS")
saveRDS(avg_batter_stats,"/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/avg_batter_stats.RDS")

avg_pitcher_stats <- readRDS("/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/avg_pitcher_stats.RDS")
avg_batter_stats <- readRDS("/Users/noahknoblauch/Dropbox/BaseballApp/Baseball_outline/data/avg_batter_stats.RDS")
write_delim(avg_batter_stats, "/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/avg_batter_stats.txt", delim = "\t")

write_delim(avg_pitcher_stats, "/Users/noahknoblauch/Desktop/BaseballApp/Baseball_outline/data/avg_pitcher_stats.txt", delim = "\t")

mean_season_stats <- season_stats %>%
  summarise(avg.Runs = mean(Runs), avg.RBI = mean(RBI), avgHR = mean(HR), avg.SB = mean(SB), avg.AVG = mean(AVG),
            avg.Wins = mean(Wins), avg.Saves = mean(Saves), avgKs = mean(Strikeouts), avg.ERA = mean(ERA), avg.WHIP = mean(WHIP))

sd_season_stats <- season_stats %>%
  summarise(sd.Runs = sd(Runs), sd.RBI = sd(RBI), sd.HR = sd(HR), sd.SB = sd(SB), sd.AVG = sd(AVG),
            sd.Wins = sd(Wins), sd.Saves = sd(Saves), sd.Ks = sd(Strikeouts), sd.ERA = sd(ERA), sd.WHIP = sd(WHIP))
roster <- read_delim("/Users/noahknoblauch/Baseball/full_roster.txt", delim = "\t")
Noah <- roster %>%
  filter(team_name == "Griffey Jr", week == 10)

team_names <- read_delim("/Users/noahknoblauch/Baseball/Team_Names.txt", delim = "\t")
roster <- inner_join(roster,team_names , by = "team_name")

roster <- roster %>%
  select(-player_key, -team_name, -team_key, -team)
names(roster)[4] <- "Manager"

all_week_player_stats <- read_delim("/Users/noahknoblauch/Dropbox/Baseball/all_week_player_stats.txt", delim = "\t")


test_all_stat <- all_week_player_stats %>%
  spread(stat_name, stat_value )

all_week_player_stats <- test_all_stat[,c("real_name","week","player_name","display_position","Runs","RBI","HR",
                                  "SB","AVG","Wins","Saves","Strikeouts","ERA","WHIP")]

test_4.g <- test_all_stat %>%
  spread(stat,value,-player_name)

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

 t <- gather(stocks, stock, price, -time)

