#first install the weird version of httr
devtools::install_github("ctrombley/httr")

library(httr)
library(xml2)
library(jsonlite)
library(ggplot2)
library(httpuv)

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
                        use_basic_auth = TRUE, cache = TRUE)

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



team_df <- group_by(team_key_df,team_key) %>% do(mutate(.,team_name=get_team_name(.$team_key,token)))

test_team_meta <- read_xml(as.character(GET(team_key_df$metadata_url[1],config(token=token))))


