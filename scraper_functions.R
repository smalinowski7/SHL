###########################################
### -------- SCRAPER FUNCTIONS -------- ###
###########################################

library(tidyverse)
library(httr)
library(jsonlite)


##################
### From Index ###
##################


# Scrape player stats
index_player_stats <- function(seasons, league = 0, type = "regular") {
  
  stats_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/players/stats", query = list(season = i, type = type, league = league))
    stats <- fromJSON(rawToChar(temp_url$content))
    
    stats <- stats %>%
      mutate(pos = case_when(position %in% c("LD", "RD") ~ "Defense",
                             position %in% c("LW", "C", "RW") ~ "Forward"),
             
             pos_broad = case_when(position %in% c("LD", "RD") ~ "Defense",
                                   position %in% c("LW", "RW") ~ "Wing",
                                   position == "C" ~ "Center"),
             
             pos = factor(pos, levels = c("Forward", "Defense")),
             
             pos_broad = factor(pos_broad, levels = c("Center", "Wing", "Defense"))) 
    
    stats_list[[i]] <- stats
  } 
  
  player_stats <- do.call(bind_rows, stats_list)
  player_stats <- unnest(player_stats, 
                         cols = c(advancedStats),
                         names_sep = "_")
  
  return(player_stats)
}



# Scrape player ratings
index_player_ratings <- function(seasons, league = 0, type = "regular") {
  
  ratings_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/players/ratings", query = list(season = i, type = type, league = league))
    ratings <- fromJSON(rawToChar(temp_url$content))
    
    ratings <- ratings %>%
      mutate(pos = case_when(position %in% c("LD", "RD") ~ "Defense",
                             position %in% c("LW", "C", "RW") ~ "Forward"),
             
             pos_broad = case_when(position %in% c("LD", "RD") ~ "Defense",
                                   position %in% c("LW", "RW") ~ "Wing",
                                   position == "C" ~ "Center"),
             
             pos = factor(pos, levels = c("Forward", "Defense")),
             
             pos_broad = factor(pos_broad, levels = c("Center", "Wing", "Defense"))) 
    
    ratings_list[[i]] <- ratings
  } 
  
  player_ratings <- do.call(bind_rows, ratings_list)

  return(player_ratings)
}



# Scrape goalie ratings
index_goalie_ratings <- function(seasons, league = 0, type = "regular") {
  
  goalie_ratings_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/goalies/ratings", query = list(season = i, type = type, league = league))
    ratings <- fromJSON(rawToChar(temp_url$content))
    
    ratings <- ratings %>%
      mutate(pos = "Goalie")
    
    goalie_ratings_list[[i]] <- ratings
  } 
  
  goalie_ratings <- do.call(bind_rows, goalie_ratings_list)
  
  return(goalie_ratings)
}



# Scrape standings
index_standings <- function(seasons, league = 0, type = "regular") {
  
  standings_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/standings", query = list(season = i, type = type, league = league))
    standings <- fromJSON(rawToChar(temp_url$content))
    standings$season <- i
    standings_list[[i]] <- standings
  } 
  
  standings <- do.call(bind_rows, standings_list)
  standings <- unnest(standings,
                      cols = c(home, away, shootout),
                      names_sep = "_")
  
  return(standings)
}



#Scrape schedule
index_schedule <- function(seasons, league = 0, type = "Regular Season") {
  
  schedule_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/schedule", query = list(season = i, type = type, league = league))
    schedule <- fromJSON(rawToChar(temp_url$content))
    schedule$season <- i
    schedule_list[[i]] <- schedule
  } 
  
  schedule <- do.call(bind_rows, schedule_list)
  # schedule <- unnest(schedule,
  #                     cols = c(home, away, shootout),
  #                     names_sep = "_")
  
  return(schedule)
}



#Scrape team meta
index_meta <- function(seasons, league = 0, type = "Regular Season") {
  
  meta_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/teams", query = list(season = i, type = type, league = league))
    meta <- fromJSON(rawToChar(temp_url$content))
    meta$season <- i
    meta_list[[i]] <- meta
  } 
  
  meta <- do.call(bind_rows, meta_list)
  meta <- unnest(meta,
                      cols = c(nameDetails, colors, stats),
                      names_sep = "_")
  
  return(meta)
}


#########################
### From .csv exports ###
#########################

#scrape boxscores
file_boxscores <- function(seasons, league = "shl") {
  
  if (!(league %in% c("shl", "smjhl"))) {
    return("Choose either shl or smjhl for league")
  }
  
  if (min(seasons) < 66) {
    return("Boxscores not available for seasons before S66")
  }
  
  
  boxscore_list <- list()
  
  for (i in seasons) {
    print(i)
    df <- read.csv(paste0("https://simulationhockey.com/games/", league, "/S", i, "/csv/boxscore_skater_summary.csv"),
                   sep = ";")
    df$season <- i
    boxscore_list[[i]] <- df
 
  }
  
  boxscores <- do.call(bind_rows, boxscore_list)
  
  return(boxscores)
}


#scrape scoring summary
file_scoring_summary <- function(seasons, league = "shl") {
  
  if (!(league %in% c("shl", "smjhl"))) {
    return("Choose either shl or smjhl for league")
  }
  
  if (min(seasons) < 66) {
    return("Boxscores not available for seasons before S66")
  }
  
  
  scoring_summary_list <- list()
  
  for (i in seasons) {
    print(i)
    df <- read.csv(paste0("https://simulationhockey.com/games/", league, "/S", i, "/csv/boxscore_period_scoring_summary.csv"),
                   sep = ";")
    df$season <- i
    scoring_summary_list[[i]] <- df
    
  }
  
  scoring_summary <- do.call(bind_rows, scoring_summary_list)
  
  return(scoring_summary)
}




#scrape goalie summary
file_goalie_summary <- function(seasons, league = "shl") {
  
  if (!(league %in% c("shl", "smjhl"))) {
    return("Choose either shl or smjhl for league")
  }
  
  if (min(seasons) < 66) {
    return("Boxscores not available for seasons before S66")
  }
  
  
  goalie_summary_list <- list()
  
  for (i in seasons) {
    print(i)
    df <- read.csv(paste0("https://simulationhockey.com/games/", league, "/S", i, "/csv/boxscore_goalie_summary.csv"),
                   sep = ";")
    df$season <- i
    goalie_summary_list[[i]] <- df
    
  }
  
  goalie_summary <- do.call(bind_rows, goalie_summary_list)
  
  return(goalie_summary)
}



#scrape team lines
file_team_lines <- function(seasons, league = "shl") {
  
  if (!(league %in% c("shl", "smjhl"))) {
    stop("Choose either shl or smjhl for league")
  }
  
  if (min(seasons) < 66) {
    stop("Team lines not available for seasons before S66")
  }
  
  
  team_lines_list <- list()
  
  for (i in seasons) {
    print(i)
    df <- read_delim(paste0("https://simulationhockey.com/games/", league, "/S", i, "/csv/team_lines.csv"),
                   delim = ";")
                  
    df$season <- i
    team_lines_list[[i]] <- df
    
  }
  
  team_lines_summary <- do.call(bind_rows, team_lines_list)
  
  return(team_lines_summary)
}



###################
### From portal ###
###################

# Scrape portal for players
portal_players <- function() {
  portal <- GET("http://portal.simulationhockey.com/api/v1/player")
  portal <- fromJSON(rawToChar(portal$content))
  
  return(portal)
}
