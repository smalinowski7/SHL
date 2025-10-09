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
    ratings_list[[i]] <- ratings
  } 
  
  player_ratings <- do.call(bind_rows, ratings_list)
  player_ratings <- do.call(bind_cols, player_ratings)
  
  return(player_ratings)
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
  standings <- unnest(cols = c(home, away, shootout),
                      names_sep = "_")
  
  return(standings)
}




https://index.simulationhockey.com/api/v1/standings