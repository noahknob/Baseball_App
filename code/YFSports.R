#first install the weird version of httr
devtools::install_github("ctrombley/httr")
library(tidyverse)
library(dplyr)
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
if(file.exists(".httr-oauth")){
  file.remove(".httr-oauth")
}
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



######### This funtion creates a data frame of team name with the corresponding team_key of each fantasy team #############
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

########### This function creates a data frame of the stat names with its corresponing stat id number#######

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

######### This funtion also gets the team name of each fantasy team (Dont think I need anymore) #############
get_team_name <- function(team_key,token){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  metadata_url=paste0(ff_base,"/team/",team_key,"/metadata/?format=xml")
  test_team_meta <- read_xml(as.character(GET(metadata_url,config(token=token)))) %>% xml_ns_strip()
  team_name <- xml_find_all(test_team_meta,"//name") %>% xml_text()
  return(team_name)
}


###### This function grabs the total stats from each team each week  ########################
get_team_week_stats <- function(team_key,week,config){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  stat_url=paste0(ff_base,"/team/",team_key,"/stats;type=week;week=",week)
  stat_res<- GET(stat_url,config)
  stat_xml <- read_xml(as.character(stat_res)) %>% xml_ns_strip() %>% xml_find_all("//stat")  %>% map_df(~data_frame(
    stat_id=xml_text(xml_find_all(.x,"stat_id")),
    stat_value=xml_text(xml_find_all(.x,"value")))) %>% mutate(week=week,team=team_key)
  return(stat_xml)
}

############# This function creates a data frame of each teams roster each week. ###########################
########### This function is now redundant because of player_stats_week function ########################

pull_roster <- function(team_key,week,config){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  roster_url=paste0(ff_base,"/team/",team_key,"/roster;week=",week)
  roster_res <- GET(roster_url,config)
  roster_xml <- read_xml(as.character(roster_res))%>%xml_ns_strip()
  roster <- xml_find_all(roster_xml,"//roster")%>%
    xml_find_all("//player")
  Player_Roster <- data_frame(player=xml_text(xml_find_all(roster,"//full")),
                                player_key=xml_text(xml_find_all(roster,"player_key")),
                                Position=xml_text(xml_find_all(roster,"//selected_position")%>%
                                                    xml_find_all("position")),
                                week=week,
                                team=team_key)
    return(Player_Roster)

}

########## This function grabs the total player stats from each week from each team ##################
player_stats_weeks <- function(team_key,week,config){
  stat_url=paste0(ff_base,"/team/",team_key,"/roster;week=",week,"/players/stats;type=week;week=",week)
  stat_res<- GET(stat_url,config)
  stat_xml <- read_xml(as.character(stat_res)) %>% xml_ns_strip()#%>%xml_find_all("//full")
  player_xml <- xml_find_all(stat_xml,"//player") %>% xml_ns_strip()

  generate_player_stats <- function(player_xml){
    nx <- player_xml
    player_name <- xml_find_first(nx,"name/full") %>% xml_text
    player_key <- xml_find_first(nx,"player_key") %>% xml_text
    week <- xml_find_first(nx,"player_stats/week") %>% xml_integer()
    display_position <- xml_find_first(nx,"display_position") %>% xml_text
    stat_id <- xml_find_all(nx,"player_stats/stats/stat/stat_id") %>% xml_integer()
    stat_value <- xml_find_all(nx,"player_stats/stats/stat/value") %>% xml_text()
    return(data_frame(player_name=player_name,
                      player_key=player_key,
                      week=week,
                      display_position=display_position,
                      stat_id=stat_id,
                      stat_value=stat_value))
  }
  player_stats <- map_df(player_xml,generate_player_stats)

}

######### This function will grab stats from a certain player for any given date##################
########### Can pull stats from every player for every day #######################
############## Have not used yet becasue of long run time. approx 24 hrs ##############

pull_daily_stats <- function(player_key,date,config){
  player_stat_url <- paste0(ff_base,"/player/",player_key,"/stats;type=date;date=",date)
  player_res <- GET(player_stat_url,config)
  player_stat_xml <- read_xml(as.character(player_res))%>%xml_ns_strip()%>% xml_find_all("//stat")  %>% map_df(~data_frame(
    stat_id=xml_text(xml_find_all(.x,"stat_id")),
    stat_value=xml_text(xml_find_all(.x,"value")),
    player_key=player_key,
    Date=date))

  return(player_stat_xml)

}


##################################################################################################

pull_stats <- function(player_key,config) {
  player_stat_url <- paste0(ff_base,"/player/",player_key,"/stats")
  player_res <- GET(player_stat_url,config)
  player_stat_xml <- read_xml(as.character(player_res)) %>% xml_ns_strip() %>%  xml_find_all("//stat")  %>% map_df(~data_frame(
    stat_id=xml_text(xml_find_all(.x,"stat_id")),
    stat_value=xml_text(xml_find_all(.x,"value")),
    player_key = player_key))

  return(player_stat_xml)

}
#####################################This Chunk of Code makes data frame of season stats for every player used #########
# full_roster <-  read_delim("/Users/noahknoblauch/Baseball/full_roster.txt",delim = "\t",guess_max = 10000)
# players_used <- full_roster %>%
#      distinct(player_key,player)
#
# full_stats <- group_by(players_used,player_key) %>% do(pull_stats(.$player_key,config)) %>% ungroup()
#
# full_stats <- inner_join(players_used,full_stats, by = "player_key")
#
# stat_categories <- read_delim("/Users/noahknoblauch/Baseball/stat_categories.txt",delim = "\t")
#
# full_stats <- full_stats %>%
#   mutate(stat_id = as.integer(stat_id))
# full_stats <- inner_join(full_stats,stat_categories, by = "stat_id")
# saveRDS(full_stats,file = "~/Desktop/BaseballApp/Baseball_outline/data/full_stats.RDS")
###############################################################################################################################

team_df <- pull_team_names(league.key,config)
write_delim(team_df,"/Users/noahknoblauch/Baseball/team_id.txt",delim="\t")

stat_categories <- pull_stat_categories(config)

write_delim(stat_categories,"/Users/noahknoblauch/Baseball/stat_categories.txt",delim="\t")

weeks <- data_frame(week=1:24)
weeks <- weeks %>% mutate(c=1)
team_df <- team_df %>% mutate(c=1)
team_df <- inner_join(weeks,team_df,by="c")



######################### This section of code grabs the roster of each team from every week ##################################
####################### Already done and saved data frame, and now redundant becasue of all_week_player_stats ################
# full_roster <- group_by(team_df,team_key,team_name,week) %>% do(pull_roster(.$team_key,.$week,config))%>%ungroup()
#
# write_delim(full_roster,"/Users/noahknoblauch/Baseball/full_roster.txt",delim = "\t")
# full_roster <-  read_delim("/Users/noahknoblauch/Baseball/full_roster.txt",delim="\t",guess_max=10000)
################################################################################################################################


# ############This Section of code is for pulling every stat from every day of the regular season from every player used##########
# ############ Still Have not done needs approx 24 hrs to complete. May not be able to becuase of too many API calls ##########################
# players_used <- full_roster%>%
#   distinct(player_key,player)
#
# regular_season <-data_frame(Dates=seq(as.Date("2017-04-02"),as.Date("2017-10-01"),by="day"))
# regular_season <-mutate(regular_season,c=1)
#
# players_used <- mutate(players_used,c=1)
#
# roster_with_dates <- inner_join(players_used,regular_season,by="c")
#
# full_stats <- group_by(roster_with_dates,player_key,Dates) %>% do(pull_daily_stats(.$player_key,.$Dates,config))%>%ungroup()
################################################################################################################################


############################# This section of code grabs player stats from each team each week  ###############################
########################################### Also Manipulates data frame to be a little neater  ##########################################
############################################ Already done and saved data frame ########################################
# all_week_player_stats <- group_by(team_df,team_key,team_name,week) %>% do(player_stats_weeks(.$team_key,.$week,config))%>%ungroup()
#
# names <-read_delim("/Users/noahknoblauch/Baseball/Team_Names.txt",delim = "\t")
# all_week_player_stats <- inner_join(all_week_player_stats,names,by="team_name")%>%
#   mutate(stat_id=as.integer(stat_id))
# all_week_player_stats <- inner_join(all_week_player_stats,stat_categories,by="stat_id")
#
# all_week_player_stats <- all_week_player_stats%>%
#   mutate(stat_name=ifelse(stat_name=="(Walks + Hits)/ Innings Pitched","WHIP",stat_name))%>%
#   select(-team_name,-team_key,-stat_id,-player_key)
#
# all_week_player_stats <- all_week_player_stats%>%
#   mutate(stat_name=ifelse(stat_name=="Innings Pitched","IP",stat_name))
#
# all_week_player_stats <- all_week_player_stats%>%
#   mutate(stat_name=ifelse(stat_name=="Batting Average","AVG",stat_name))
#
# all_week_player_stats<- all_week_player_stats%>%
#   mutate(stat_name=ifelse(stat_name=="Earned Run Average","ERA",stat_name))

all_week_player_stats <- all_week_player_stats%>%
  mutate(stat_name=ifelse(stat_name=="Runs Batted In","RBI",stat_name))%>%
  mutate(stat_name=ifelse(stat_name=="Hits / At Bats","H/AB",stat_name))%>%
  mutate(stat_name=ifelse(stat_name=="Home Runs","HR",stat_name))%>%
  mutate(stat_name=ifelse(stat_name=="Stolen Bases","SB",stat_name))



write_delim(all_week_player_stats,"/Users/noahknoblauch/Baseball/all_week_player_stats.txt",delim = "\t")



######################## This section of ccode grabs the total week stats from every team ###############################
####################### Manipulates data frame so team name is replaced with real name ###########################
######################## Changes names of a few stats like WHIP and ERA ########################################
###################################### Already done and saved data frame #######################################

# all_week_stats <- group_by(team_df,team_key,team_name,week) %>% do(get_team_week_stats(.$team_key,.$week,config))%>%ungroup()
#
# all_week_stats <- inner_join(stat_categories,all_week_stats,by="stat_id")
#
# names <-read_delim("/Users/noahknoblauch/Baseball/Team_Names.txt",delim = "\t")
# all_week_stats <- inner_join(all_week_stats,names,by="team_name")
#
# all_week_stats <- all_week_stats%>%
#   mutate(Stat_Name=ifelse(stat_name=="(Walks + Hits)/ Innings Pitched","WHIP",stat_name))%>%
#   select(-team_name,-team,-team_key,-stat_id,-stat_name)
#
# all_week_stats <- all_week_stats%>%
#   mutate(stats=ifelse(Stat_Name=="Innings Pitched","IP",Stat_Name))%>%
#   select(-Stat_Name)
#
# all_week_stats <- all_week_stats%>%
#   mutate(Stat=ifelse(stats=="Batting Average","AVG",stats))%>%
#   select(-stats)
#
# all_week_stats<- all_week_stats%>%
#   mutate(Stat=ifelse(Stat_Name=="Earned Run Average","ERA",Stat_Name))%>%
#   select(-Stat_Name)
#
#
# write_delim(all_week_stats,"/Users/noahknoblauch/Baseball/all_week_stats.txt",delim = "\t")

stats<-read_delim("/Users/noahknoblauch/Baseball/all_week_stats.txt",delim="\t",guess_max=10000)
