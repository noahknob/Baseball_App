#first install the weird version of httr
devtools::install_github("ctrombley/httr")
library(tidyverse)
library(httr)
library(xml2)
library(jsonlite)
library(ggplot2)
library(httpuv)
library(htmltidy)

#make sure that when you type `getwd()`, the current directory is `code`
#You'll need to get auth.json
stopifnot(file.exists("../data/auth.json"))
authf <- "../data/auth.json"
auth_dat <- fromJSON(authf)

#3 steps to making a auth.json file
#1) get consumer_secret and consumer_key from: https://developer.yahoo.com/apps/YWVpEk5c/
#2) create a list with key and secret x <- list(consumer_secret="MYCONSUMERSECRET",consumer_key="MYCONSUMERKEY)
#3) create a json string from the list json_txt <- toJSON(x,auto_unbox=T)
#4) write the json string to a file write(json_txt,"../data/auth.json")


# json_txt <- toJSON(,auto_unbox = T)

#You could also do it this way

#3)

endpoint <- oauth_endpoint("get_request_token", "request_auth", "get_token",
                           base_url = "https://api.login.yahoo.com/oauth2")

app <- oauth_app("yahoo",
                 key = auth_dat["consumer_key"],
                 secret = auth_dat["consumer_secret"],
                 redirect_uri = "oob")

token <- oauth2.0_token(endpoint, app, use_oob = TRUE, as_header = TRUE,
                        use_basic_auth = TRUE)

config <-  httr::config(token = token)



ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
ff.url <- paste0(ff_base,"/game/mlb?format=json")
game.key.json <- fromJSON(as.character(GET(ff.url, config)))
game.key <- game.key.json$fantasy_content$game["game_key"]
# my personal leagueid, you will have to use your own, mine is private
# game_met <- paste0(ff_base,"/game/",game.key,"/?format=json")
# a_leagues <- GET(leagues_url,config(token=token))
# fromJSON(as.character(a_leagues))
league.id <- "64399"
league.key <- paste0(game.key, ".l.", league.id)
# league.base <- "https://fantasysports.yahooapis.com/fantasy/v2/league/"
# league_url <- paste0(league.base,league.key)
# league_dat <- GET(paste0(league_url,"/league/?format=xml"),config)
# league_dat_xml <- read_xml(as.character(league_dat))

pull_team_names <- function(league.key,config){

  league_url <- sprintf("https://fantasysports.yahooapis.com/fantasy/v2/league/%s/teams/metadata?format=xml", league.key)
  teams_dat <- GET(league_url,config)
  teams_xml <- read_xml(as.character(teams_dat)) %>% xml_ns_strip()
  all_teams <- xml_find_all(teams_xml,"//team")
  team_df <- data_frame(
      team_key=xml_text(xml_find_all(all_teams,"team_key")),
    team_id=xml_text(xml_find_all(all_teams,"team_id")),
    team_name=xml_text(xml_find_all(all_teams,"name")),
  )
  return(team_df)
}


test_name <- data_frame(Team_Name=xml_text(xml_find_all(teams_xml,"///name")))#,TEam_ID=xml_text(xml_find_all(teams_xml,"//team_id")))




pull_stat_categories<- function(config){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  stat_url=paste0(ff_base,"/game/370/stat_categories/?format=xml")
  stat_categories <- read_xml(as.character(GET(stat_url,config))) %>% xml_ns_strip()
  all_stats <- xml_find_all(stat_categories,"//stat")
  all_names <- all_stats %>% map_df(~data_frame(
    stat_name=xml_text(xml_find_all(.x,"name")),
    stat_id=xml_text(xml_find_all(.x,"stat_id"))))
  return(all_names)
}
get_team_name <- function(team_key,token){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  metadata_url=paste0(ff_base,"/team/",team_key,"/metadata/?format=xml")
  test_team_meta <- read_xml(as.character(GET(metadata_url,config(token=token)))) %>% xml_ns_strip()
  team_name <- xml_find_all(test_team_meta,"//name") %>% xml_text()
  return(team_name)
}
get_team_week_stats <- function(team_key,week,config){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  stat_url=paste0(ff_base,"/team/",team_key,"/stats;type=week;week=",week)
  stat_res<- GET(stat_url,config)
  stat_xml <- read_xml(as.character(stat_res)) %>% xml_ns_strip() %>% xml_find_all("//stat")  %>% map_df(~data_frame(
    stat_id=xml_text(xml_find_all(.x,"stat_id")),
    stat_value=xml_text(xml_find_all(.x,"value")))) %>% mutate(week=week,team=team_key)
  return(stat_xml)
}



stat_url_test <- paste0(ff_base,"/team/","370.l.64399.t.10","/stats;type=week;week=","14")
stat_res_test <- GET(stat_url_test,config)
stat_xml_test <-read_xml(as.character(stat_res_test))



team_df <- pull_team_names(league.key,config)
stat_categories <- pull_stat_categories(config)

weeks <- data_frame(week=1:24)
weeks <- weeks %>% mutate(c=1)
team_df <- team_df %>% mutate(c=1)
team_df <-inner_join(weeks,team_df,by="c")
all_week_stats <- group_by(team_df,team_key,team_name,week) %>% do(get_team_week_stats(.$team_key,.$week,config))%>%ungroup()

all_week_stats <- inner_join(stat_categories,all_week_stats,by="stat_id")

names <-read_delim("/Users/noahknoblauch/Baseball/Team_Names.txt",delim = "\t")
all_week_stats <- inner_join(all_week_stats,names,by="team_name")

all_week_stats <- all_week_stats%>%
  mutate(Stat_Name=ifelse(stat_name=="(Walks + Hits)/ Innings Pitched","WHIP",stat_name))%>%
  select(-team_name,-team,-team_key,-stat_id,-stat_name)

all_week_stats <- all_week_stats%>%
  mutate(stats=ifelse(Stat_Name=="Innings Pitched","IP",Stat_Name))%>%
  select(-Stat_Name)

all_week_stats <- all_week_stats%>%
  mutate(Stat=ifelse(stats=="Batting Average","AVG",stats))%>%
  select(-stats)

all_week_stats<- all_week_stats%>%
  mutate(Stat=ifelse(Stat_Name=="Earned Run Average","ERA",Stat_Name))%>%
  select(-Stat_Name)


write_delim(all_week_stats,"/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t")




