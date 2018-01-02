# Code

The most difficult part of this project was gathering all the data from my Yahoo fantasy league.  In order to gain acess to the data in my fantasy league I needed to acess Yahoo's API

I created a JSON file with my yahoo consumer key and consumer secret

```
authf <- "../data/auth.json"
auth_dat <- fromJSON(authf)

```
To create the auth.JSON file one must get consumer_secret and consumer_key from: https://developer.yahoo.com/apps/YWVpEk5c/

Then create a list with the key and secret, convert the list to JSON string and write that JSON string as a file

```
x <- list(consumer_secret="MYCONSUMERSECRET",consumer_key="MYCONSUMERKEY)
json_txt <- toJSON(x,auto_unbox=T)
write(json_txt,"../data/auth.json")

```

Yahoo's API uses the Oauth protocol.  Oauth allows users to share their yahoo informaton without having to give out their Username and password.  


```
endpoint <- oauth_endpoint("get_request_token", "request_auth", "get_token",
                           base_url = "https://api.login.yahoo.com/oauth2")
app <- oauth_app("yahoo",
                 key = auth_dat["consumer_key"],
                 secret = auth_dat["consumer_secret"],
                 redirect_uri = "oob")

token <- oauth2.0_token(endpoint, app, use_oob = TRUE, as_header = TRUE,
                        use_basic_auth = TRUE)

config <-  httr::config(token = token)

```



```
ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
ff.url <- paste0(ff_base,"/game/mlb?format=json")
game.key.json <- fromJSON(as.character(GET(ff.url, config)))
game.key <- game.key.json$fantasy_content$game["game_key"]`
```

To get info from your specific league you have to use your league id.
```
league.id <- "MyLeagueID"
league.key <- paste0(game.key, ".l.", league.id)
```

Once you have acess to your league you can pull any information from the Yahoo API.  all the info comes in JSON which can be can of hard to interpret but you can translate it all to xml which is a bit easier to understand.  


```
get_team_week_stats <- function(team_key,week,config){
  ff_base <- "https://fantasysports.yahooapis.com/fantasy/v2"
  stat_url=paste0(ff_base,"/team/",team_key,"/stats;type=week;week=",week)
  stat_res<- GET(stat_url,config)
  stat_xml <- read_xml(as.character(stat_res)) %>% xml_ns_strip() %>% xml_find_all("//stat")  %>% map_df(~data_frame(
    stat_id=xml_text(xml_find_all(.x,"stat_id")),
    stat_value=xml_text(xml_find_all(.x,"value")))) %>% mutate(week=week,team=team_key)
  return(stat_xml)
}
```

The files here are scripts that are run from the command line. It is often
useful for these scripts to take arguments as input. Several examples of how to
read command line arguments are provided, specifically in R, Python, and Bash. For practice, you can pass arguments to these functions.

```
$ code/script.R arg1 arg2 arg3
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

To be able to use these scripts from anywhere, first make them executable, e.g.

```
$ chmod +x script.R
```

Next open your `.bashrc` file and add the following line to include the scripts in `code` in the `PATH`. The example below demonstrates how to do this for a project called `example` in the home directory.

```
export PATH=$PATH:~/example/code
```

If you do this, you no longer need to provide the path to the executable file.

```
$ script.R arg1 arg2 arg3
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```
