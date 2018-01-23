library(tidyverse)

stats_file <- "/Users/noahknoblauch/Baseball/DirtyStats/Week7stats.txt"
stats_file2 <- "/Users/noahknoblauch/Baseball/DirtyStats/Week8stats.txt"

rootdirectory <- "/Users/noahknoblauch/Baseball/DirtyStats"
filenames <- Sys.glob(file.path(rootdirectory,"*txt"))

filenew_stats_file <- "~/Baseball/CleanWeek20stats.txt"

stats_table <- function(statsfile)
{
  return(read.table(statsfile,
                    col.names = c("runs","hr","rbi","sb","avg","wins","sv","K","ERA","WHIP")))
}
week <- stats_table(stats_file)
week2 <- stats_table(stats_file2)

names_table <- function(week)
{
  return(mutate(week,Teams=c("Ryan","Austin","Owen","Noah","Matt","ED","Dan","Jordan","Wyatt","Josh")))
}
team_stats <- names_table(week)
team_stats2 <- names_table(week2)

new_file <- function(team)
write_delim(team_stats,path=new_stats_file,delim="\t")
team_stats <- read_delim(new_stats_file,delim="\t")


Total_Roto <- function(team_stats)
{

#Creates the batting roto scoreboard
scoreboard <- mutate(team_stats,Batting=rank(runs)+
                      rank(hr)+
                       rank(rbi)+
                       rank(sb)+
                       rank(avg),
                     Pitching=rank(wins)+
                       rank(sv)+
                       rank(K)+
                       rank(-ERA)+
                       rank(-WHIP))
Batting_Scoreboard <- scoreboard %>%
  arrange(desc(Batting)) %>% select(Teams,Batting)

Pitching_Scoreboard <- scoreboard %>%
  arrange(desc(Pitching)) %>% select(Teams,Pitching)

Final_ROTO_ScoreBoard <- inner_join(Pitching_Scoreboard,Batting_Scoreboard,by="Teams")
Final_ROTO_ScoreBoard <- mutate(Final_ROTO_ScoreBoard,Total=Pitching+Batting)
Final_ROTO_ScoreBoard <- arrange(Final_ROTO_ScoreBoard, desc(Total))

return(Final_ROTO_ScoreBoard)
}



Teams <- select(team_stats,Teams)

one_week <- select(team_stats2,-Teams)-select(team_stats,-Teams)

one_week <- mutate(one_week,Teams)

week1 %>% filter(stat_name=="Stolen Bases") %>% mutate(roto_points=rank(stat_value))
week1 <- week1 %>% group_by(stat_name)%>% mutate(roto_points=rank(stat_value))
