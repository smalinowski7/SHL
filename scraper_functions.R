###########################################
### -------- SCRAPER FUNCTIONS -------- ###
###########################################


### To do ###

# fix the hard coded paths when append = T
# in player and goalie stats
# for playoffs


library(tidyverse)
library(httr)
library(jsonlite)
library(here)


shl_save_path <- here("Data/SHL")
smjhl_save_path <- here("Data/SMJHL")
save_path_numeric <- c(shl_save_path, smjhl_save_path)
save_path_character <- c(shl_save_path, smjhl_save_path)
names(save_path_character) <- c("shl", "smjhl")



##################################################
### Update and save new season's worth of data ###
##################################################

# season_to_add <- 88
# league_char <- "shl"
# league_numeric <- 0
# directory <- "SHL"
# 
# 
# bs <- file_boxscores(season_to_add, league = league_char, append = T)
# gs <- file_goalie_summary(season_to_add, league = league_char, append = T)
# lines <- file_team_lines(season_to_add, league = league_char, append = T)
# ss <- file_scoring_summary(season_to_add, league = league_char, append = T)
# gr <- index_goalie_ratings(season_to_add, league = league_numeric, append = T)
# gs <- index_goalie_stats(season_to_add, league = league_numeric, append = T)
# pr <- index_player_ratings(season_to_add, league = league_numeric, append = T)
# ps <- index_player_stats(season_to_add, league = league_numeric, append = T)
# sch <- index_schedule(season_to_add, league = league_numeric, append = T)
# stand <- index_standings(season_to_add, league = league_numeric, append = T)
# meta <- index_meta(season_to_add, league = league_numeric, append = T)
# 
# # Some day I will include playoffs in the scraper functions
# # For now, these are the only ones I use so they're hard-coded in
# gs_po_new <- index_goalie_stats(season_to_add, league = league_numeric, type = "playoffs", append = T)
# ps_po_new <- index_player_stats(season_to_add, league = league_numeric, type = "playoffs", append = T)
# sch_po_new <- index_schedule(season_to_add, league = league_numeric, type = "playoffs", append = T)
# 
# gs_po_old <- read_csv(paste0("Data/", directory, "/Playoffs/index_goalie_stats.csv"))
# ps_po_old <-read_csv(paste0("Data/", directory, "/Playoffs/index_player_stats.csv"))
# sch_po_old <-read_csv(paste0("Data/", directory, "/Playoffs/index_schedule.csv"))
# 
# gs_po <- rbind(gs_po_old, gs_po_new)
# ps_po <- rbind(ps_po_old, ps_po_new)
# sch_po <- rbind(sch_po_old, sch_po_new)
# 
# # Check unqiue seasons
# unique(bs$season)
# unique(gs$season)
# unique(lines$season)
# unique(ss$season)
# unique(gr$season)
# unique(gs$season)
# unique(pr$season)
# unique(ps$season)
# unique(sch$season)
# unique(stand$season)
# unique(meta$season)
# unique(gs_po$season)
# unique(ps_po$season)
# unique(sch_po$season)
# 
# 
# # Save
# write_csv(bs, paste0("Data/", directory, "/file_boxscore.csv"))
# write_csv(gs, paste0("Data/", directory, "/file_goalie_summary.csv"))
# write_csv(lines, paste0("Data/", directory, "/file_lines.csv"))
# write_csv(ss, paste0("Data/", directory, "/file_scoring_summary.csv"))
# write_csv(gr, paste0("Data/", directory, "/index_goalie_ratings.csv"))
# write_csv(gs, paste0("Data/", directory, "/index_goalie_stats.csv"))
# write_csv(pr, paste0("Data/", directory, "/index_player_ratings.csv"))
# write_csv(ps, paste0("Data/", directory, "/index_player_stats.csv"))
# write_csv(sch, paste0("Data/", directory, "/index_schedule.csv"))
# write_csv(stand, paste0("Data/", directory, "/index_standings.csv"))
# write_csv(meta, paste0("Data/", directory, "/index_team_meta.csv"))
# 
# write_csv(gs_po, paste0("Data/", directory, "/Playoffs/index_goalie_stats.csv"))
# write_csv(ps_po, paste0("Data/", directory, "/Playoffs/index_player_stats.csv"))
# write_csv(sch_po, paste0("Data/", directory, "/Playoffs/index_schedule.csv"))






##################
### From Index ###
##################


# Scrape player stats
index_player_stats <- function(seasons, league = 0, type = "regular", append = FALSE) {
  
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
  
  
  if (append == TRUE) {
    merged_player_stats <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_player_stats.csv")), player_stats)
    return(merged_player_stats)
  }
  
  return(player_stats)
}



# Scrape player ratings
index_player_ratings <- function(seasons, league = 0, type = "regular", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_player_ratings <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_player_ratings.csv")), player_ratings)
    return(merged_player_ratings)
  }

  return(player_ratings)
}



# Scrape goalie stats
index_goalie_stats <- function(seasons, league = 0, type = "regular", append = FALSE) {
  
  goalie_stats_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/goalies/stats", query = list(season = i, type = type, league = league))
    goalie_stats <- fromJSON(rawToChar(temp_url$content))
    
    goalie_stats <- goalie_stats %>%
      mutate(pos = "Goalie",
             
             pos_broad = "Goalie")
    
    goalie_stats$gaa <- as.numeric(goalie_stats$gaa)
    goalie_stats$savePct <- as.numeric(goalie_stats$savePct)
             

    
    goalie_stats_list[[i]] <- goalie_stats
  } 
  
  goalie_stats <- do.call(bind_rows, goalie_stats_list)
 
  
  if (append == TRUE) {
    merged_goalie_stats <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_goalie_stats.csv")), goalie_stats)
    return(merged_goalie_stats)
  }
  
  return(goalie_stats)
}




# Scrape goalie ratings
index_goalie_ratings <- function(seasons, league = 0, type = "regular", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_goalie_ratings <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_goalie_ratings.csv")), goalie_ratings)
    return(merged_goalie_ratings)
  }
  
  return(goalie_ratings)
}



# Scrape standings
index_standings <- function(seasons, league = 0, type = "regular", append = FALSE) {
  
  standings_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/standings", query = list(season = i, type = type, league = league))
    standings <- fromJSON(rawToChar(temp_url$content))
    standings$season <- i
    standings$winPercent <- as.numeric(standings$winPercent)
    standings_list[[i]] <- standings
  } 
  
  standings <- do.call(bind_rows, standings_list)
  standings <- unnest(standings,
                      cols = c(home, away, shootout),
                      names_sep = "_")
  
  if (append == TRUE) {
    merged_standings <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_standings.csv")), standings)
    return(merged_standings)
  }
  
  return(standings)
}



#Scrape schedule
index_schedule <- function(seasons, league = 0, type = "Regular Season", append = FALSE) {
  
  schedule_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/schedule", query = list(season = i, type = type, league = league))
    schedule <- fromJSON(rawToChar(temp_url$content))
    schedule$season <- i
    schedule$slug <- as.numeric(schedule$slug)
    schedule_list[[i]] <- schedule
  } 
  
  schedule <- do.call(bind_rows, schedule_list)
  schedule$date <- as.Date(schedule$date)
  
  if (append == TRUE) {
    merged_schedule <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_schedule.csv")), schedule)
    return(merged_schedule)
  }
  
  return(schedule)
}



#Scrape team meta
index_meta <- function(seasons, league = 0, type = "Regular Season", append = FALSE) {
  
  meta_list <- list()
  
  for (i in seasons) {
    print(i)
    temp_url <- GET("https://index.simulationhockey.com/api/v1/teams", query = list(season = i, type = type, league = league))
    meta <- fromJSON(rawToChar(temp_url$content))
    meta$season <- i
    meta <- unnest(meta,
                   cols = c(nameDetails, colors, stats),
                   names_sep = "_")    
    meta$stats_winPercent <- as.numeric(meta$stats_winPercent)
    
    meta_list[[i]] <- meta
  } 
  
  meta <- do.call(bind_rows, meta_list)

  
  if (append == TRUE) {
    merged_meta <- bind_rows(read_csv(paste0(save_path_numeric[league + 1], "/index_team_meta.csv")), meta)
    return(merged_meta)
  }
  
  return(meta)
}


#########################
### From .csv exports ###
#########################

#scrape boxscores
file_boxscores <- function(seasons, league = "shl", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_boxscores <- bind_rows(read_csv(paste0(save_path_character[league], "/file_boxscore.csv")), boxscores)
    return(merged_boxscores)
  }
  
  return(boxscores)
}


#scrape scoring summary
file_scoring_summary <- function(seasons, league = "shl", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_scoring_summary <- bind_rows(read_csv(paste0(save_path_character[league], "/file_scoring_summary.csv")), scoring_summary)
    return(merged_scoring_summary)
  }
  
  return(scoring_summary)
}




#scrape goalie summary
file_goalie_summary <- function(seasons, league = "shl", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_goalie_summary <- bind_rows(read_csv(paste0(save_path_character[league], "/file_goalie_summary.csv")), goalie_summary)
    return(merged_goalie_summary)
  }
  
  return(goalie_summary)
}



#scrape team lines
file_team_lines <- function(seasons, league = "shl", append = FALSE) {
  
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
  
  if (append == TRUE) {
    merged_team_lines <- bind_rows(read_csv(paste0(save_path_character[league], "/file_lines.csv")), team_lines_summary)
    return(merged_team_lines)
  }
  
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
