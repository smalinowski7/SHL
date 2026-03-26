##############
### Notes ####
##############


# Playoff results are hard coded to start at S68 - season when most of that section was stopped

library(tidyverse)
library(httr)
library(jsonlite)
source("scraper_functions.R")


#############################
### Load and prepare data ###
#############################


# Load meta
team_meta <- read.csv("Data/SHL/index_team_meta.csv") 


team_abb_map <- team_meta %>%
  mutate(abbreviation = ifelse(abbreviation == "WKP", "BAP", abbreviation)) %>%
  group_by(abbreviation) %>%
  summarise(id = unique(id))

team_abb_map <- team_abb_map %>%
  select(id, abbreviation)


team_meta_j <- read.csv("Data/SMJHL/index_team_meta.csv") 


team_abb_map_j <- team_meta_j %>%
  mutate(abbreviation = ifelse(abbreviation == "ANA", "YUM", abbreviation)) %>%
  mutate(abbreviation = ifelse(abbreviation == "REG", "TBW", abbreviation)) %>%
  group_by(abbreviation) %>%
  summarise(id = unique(id))

team_abb_map_j <- team_abb_map_j %>%
  select(id, abbreviation)



# Scrape awwards
player_awards_link <- GET("https://portal.simulationhockey.com/api/v1/history/player")
player_awards_df <- fromJSON(rawToChar(player_awards_link$content))


team_awards_link <- GET("https://portal.simulationhockey.com/api/v1/history/team")
team_awards_df <- fromJSON(rawToChar(team_awards_link$content))


user_awards_link <- GET("https://portal.simulationhockey.com/api/v1/history/user-achievement")
user_awards_df <- fromJSON(rawToChar(user_awards_link$content))


hof_link <- GET("https://portal.simulationhockey.com/api/v1/hof")
hof_df <- fromJSON(rawToChar(hof_link$content))


# Load SHL team stats
rs_stats <- read_csv("Data/SHL/index_player_stats.csv")
po_stats <- read_csv('Data/SHL/Playoffs/index_player_stats.csv')
rs_stats_g <- read_csv("Data/SHL/index_goalie_stats.csv")
po_stats_g <- read_csv("Data/SHL/Playoffs/index_goalie_stats.csv")


# Load SMJHL team stats
rs_stats_j <- read_csv("Data/SMJHL/index_player_stats.csv")
po_stats_j <- read_csv('Data/SMJHL/Playoffs/index_player_stats.csv')
rs_stats_g_j <- read_csv("Data/SMJHL/index_goalie_stats.csv")
po_stats_g_j <- read_csv("Data/SMJHL/Playoffs/index_goalie_stats.csv")


# Load ratings and select tpe
player_ratings <- read_csv("Data/SHL/index_player_ratings.csv") %>%
  select(id, season, appliedTPE)
goalie_ratings <- read_csv("Data/SHL/index_goalie_ratings.csv") %>%
  select(id, season, appliedTPE)
ratings <- rbind(player_ratings, goalie_ratings)

j_player_ratings <- read_csv("Data/SMJHL/index_player_ratings.csv") %>%
  select(id, season, appliedTPE)
j_goalie_ratings <- read_csv("Data/SMJHL/index_goalie_ratings.csv") %>%
  select(id, season, appliedTPE)
j_ratings <- rbind(j_player_ratings, j_goalie_ratings)


# Load draft data
draft_url <- GET("https://portal.simulationhockey.com/api/v1/history/draft")
draft_df <- fromJSON(rawToChar(draft_url$content)) %>%
  filter(!(tolower(playerName) %in% c("forfeited", "forfeit")))

shl_first_round <- draft_df %>%
  filter(leagueID == 0 & round == 1)

j_first_round <- draft_df %>%
  filter(leagueID == 1 & round == 1)


# Load standings
shl_standings <- read_csv("Data/SHL/index_standings.csv")

j_standings <- read_csv("Data/SMJHL/index_standings.csv")


# Load and format playoff schedules
po_sch <- read_csv("Data/SHL/Playoffs/index_schedule.csv") %>%
  filter(type == "Playoffs")

po_list <- list()
for (i in unique(team_abb_map$id)) {
  print(i)
  temp_df <- po_sch %>%
    filter(homeTeam == i | awayTeam == i) %>%
    mutate(team = case_when(homeTeam == i ~ homeTeam,
                            awayTeam == i ~ awayTeam),
           opp = case_when(homeTeam == i ~ awayTeam,
                           awayTeam == i ~ homeTeam),
           score = case_when(homeTeam == i ~ homeScore,
                             awayTeam == i ~ awayScore),
           opp_score = case_when(homeTeam == i ~ awayScore,
                                 awayTeam == i~ homeScore),
           win = 1*(score > opp_score))
  po_list[[i+1]] <- temp_df
}
po_sch_formatted <- do.call(rbind, po_list) 
po_sch_sum <- po_sch_formatted %>%
  arrange(date) %>%
  group_by(season, team) %>%
  mutate(round = match(opp, unique(opp))) %>%
  group_by(season, team) %>%
  filter(round == max(round)) %>%
  group_by(season, team, round) %>%
  summarise(opp = opp[1],
            games_won = sum(win),
            games_lost = length(win) - games_won) %>%
  ungroup() %>%
  mutate(outcome = ifelse(games_won > games_lost, "Won", "Lost in"),
         round_name = case_when(round == 1 ~ "Round 1",
                                round == 2 ~ "Round 2",
                                round == 3 ~ "Conference Finals",
                                round == 4 ~ "Challenge Cup"),
         losing_games = ifelse(games_won == 4, games_lost, games_won)) %>%
  left_join(select(team_meta, id, name, season), by = c("opp" = "id", "season")) %>%
  rename("opp_abb" = "name") %>%
  mutate(label = paste0(outcome, " ", round_name, " against ", opp_abb, " (4-", losing_games, ")"))


# Repeat for the J
po_sch_j <- read_csv("Data/SMJHL/Playoffs/index_schedule.csv") %>%
  filter(type == "Playoffs")

po_list_j <- list()
for (i in unique(team_abb_map_j$id)) {
  print(i)
  temp_df <- po_sch_j %>%
    filter(homeTeam == i | awayTeam == i) %>%
    mutate(team = case_when(homeTeam == i ~ homeTeam,
                            awayTeam == i ~ awayTeam),
           opp = case_when(homeTeam == i ~ awayTeam,
                           awayTeam == i ~ homeTeam),
           score = case_when(homeTeam == i ~ homeScore,
                             awayTeam == i ~ awayScore),
           opp_score = case_when(homeTeam == i ~ awayScore,
                                 awayTeam == i~ homeScore),
           win = 1*(score > opp_score))
  po_list_j[[i+1]] <- temp_df
}
po_sch_formatted_j <- do.call(rbind, po_list_j) 
po_sch_sum_j <- po_sch_formatted_j %>%
  arrange(date) %>%
  group_by(season, team) %>%
  mutate(round = match(opp, unique(opp))) %>%
  group_by(season, team) %>%
  filter(round == max(round)) %>%
  group_by(season, team, round) %>%
  summarise(opp = opp[1],
            games_won = sum(win),
            games_lost = length(win) - games_won) %>%
  ungroup() %>%
  mutate(outcome = ifelse(games_won > games_lost, "Won", "Lost in"),
         round_name = case_when(round == 1 ~ "Round 1",
                                round == 2 ~ "Round 2",
                                round == 3 ~ "Conference Finals",
                                round == 4 ~ "Four Star Cup"),
         losing_games = ifelse(games_won == 4, games_lost, games_won)) %>%
  left_join(select(team_meta_j, id, name, season), by = c("opp" = "id", "season")) %>%
  rename("opp_abb" = "name") %>%
  mutate(label = paste0(outcome, " ", round_name, " against ", opp_abb, " (4-", losing_games, ")"))



draft_season_summary <- function(league, season) {
  
  
  if(league == "SHL") {
    draft_df <- shl_first_round
    abbr_map <- team_abb_map
  }
  
  if(league == "SMJHL") {
    draft_df <- j_first_round
    abbr_map <- team_abb_map_j
  }
  
  first_rounders <- draft_df %>%
    filter(seasonID == season) %>%
    mutate(int_label = ifelse(isExpansion == TRUE,
                              paste0(playerName, " (", draftNumber, " OA)*"),
                              paste0(playerName, " (", draftNumber, " OA)"))) %>%
    arrange(draftNumber) %>%
    group_by(teamID) %>%
    summarise(picks = paste0(int_label,
                             collapse = ", "))
    
    
  team_list <- abbr_map %>%
    left_join(first_rounders, by = c("id" = "teamID")) %>%
    mutate(picks = case_when(is.na(picks) ~ "No pick",
                             TRUE ~ picks)) %>%
    mutate(picks = paste0(abbreviation, " S", season, ": ", picks)) %>%
    arrange(abbreviation)
  
  
  if(league == "SHL") {
    write_lines(team_list$picks,
                paste0("Projects/History catchup and code/Draft lists/SHL_S", season, "_draft.txt"))
  }
  
  if(league == "SMJHL") {
    write_lines(team_list$picks,
                paste0("Projects/History catchup and code/Draft lists/SMJHL_S", season, "_draft.txt"))

  }
}
  
  
  
  
  
  
  # Format team first round picks
  first_rounders <- shl_first_round %>%
    filter(teamID == team_id) %>%
    arrange(seasonID) %>%
    mutate(int_label = ifelse(isExpansion == TRUE,
                              paste0(playerName, " (", draftNumber, " OA)*"),
                              paste0(playerName, " (", draftNumber, " OA)"))) %>%
    group_by(seasonID) %>%
    summarise(int_label = paste0(int_label, collapse = ", ")) 
  
  
  first_round_null <- data.frame(seasonID = min_draft_season:current_draft_season) %>%
    left_join(first_rounders) %>%
    mutate(int_label = ifelse(is.na(int_label),
                              "No pick",
                              int_label)) %>%
    arrange(seasonID) %>%
    mutate(label = paste0("S", seasonID, ": ", int_label))
}
