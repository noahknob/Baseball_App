p.stat <- read_delim("/Users/noahknoblauch/Baseball/prob_season_stats.txt",delim = "\t")

simulation <- function(num) {
  winning_values <- c()
  for (i in 1:num) {

    team_1 <- rnorm(1,mean = mean(p.stat[["AVG"]]), sd = sd(p.stat[["AVG"]]))
    team_2 <- rnorm(1,mean = mean(p.stat[["AVG"]]), sd = sd(p.stat[["AVG"]]))
    if (team_1 > team_2) {
      winning_values <- c(winning_values,team_1)
    }
    if (team_1 < team_2) {
      winning_values <- c(winning_values,team_1)
    }
  }
  return(winning_values)
}
thing <- sort(simulation(1000))

prob_295 <- pnorm(.300,mean = mean(p.stat[["AVG"]]), sd = sd(p.stat[["AVG"]])) - pnorm(.290,mean = mean(p.stat[["AVG"]]), sd = sd(p.stat[["AVG"]]))
prob_295_win <- pnorm(.300,mean = mean(thing), sd = sd(thing)) - pnorm(.290,mean = mean(thing), sd = sd(thing))

mean(thing)
hist(thing)


avgs <- rnorm(1000,mean = mean(p.stat[["AVG"]]), sd = sd(p.stat[["AVG"]]))
hist(avgs)

test
