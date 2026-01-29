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


team_meta_j <- read.csv("Data/SMJHL/index_team_meta.csv") %>%
  mutate(abbreviation = ifelse(abbreviation == "ANA", "YUM", abbreviation)) %>%
  mutate(abbreviation = ifelse(abbreviation == "REG", "TBW", abbreviation)) %>%
  group_by(abbreviation) %>%
  summarise(id = unique(id))

team_abb_map_j <- team_meta_j %>%
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

#################################
### Championship team summary ###
#################################


# Format challenge cup df
challenge_cup <- team_awards_df %>%
  filter(achievementName == "Challenge Cup") %>%
  left_join(team_abb_map, by = c("teamID" = "id"))


# Format 4 star cup df
four_star_cup <- team_awards_df %>%
  filter(achievementName == "Four Star Cup") %>%
  left_join(team_abb_map_j, by = c("teamID" = "id"))
  

player_award_winners <- player_awards_df %>%
  filter(won == TRUE)
  
  
challenge_cup_summary <- function(cup_season) {
  
  
  # Filter for winner
  winner <- challenge_cup %>%
    filter(seasonID == cup_season) %>%
    select(abbreviation) %>%
    unlist()
  
  
  # Filter award season
  award_season <- player_award_winners %>%
    filter(seasonID == cup_season) %>%
    filter(leagueID == 0) %>%
    filter(achievement %in% c(11, 12, 29, 3, 30, 31, 33, 39, 4, 41, 42, 43, 44, 45, 49, 5, 6))

  
  rs_roster <- rs_stats %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  po_roster <- po_stats %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  rs_roster_g <- rs_stats_g %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  po_roster_g <- po_stats_g %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  
  unique_roster <- rbind(rs_roster, po_roster, rs_roster_g, po_roster_g) %>%
    distinct() %>%
    mutate(season = cup_season) 
  
  
  # merge with awards
  unique_roster <- unique_roster %>%
    left_join(select(award_season, fhmID, achievementName), by = c("id" = "fhmID")) %>%
    left_join(select(hof_df, playerID, seasonInducted), by = c("id" = "playerID"))
    
  return(unique_roster)
  
  
  
}




four_star_summary <- function(cup_season) {
  
  
  # Filter for winner
  winner <- four_star_cup %>%
    filter(seasonID == cup_season) %>%
    select(abbreviation) %>%
    unlist()
  
  
  # Filter award season
  award_season <- player_award_winners %>%
    filter(seasonID == cup_season) %>%
    filter(leagueID == 1) %>%
    filter(achievement %in% c(14, 15, 16, 18, 21, 25, 27, 28, 3, 32, 35, 36, 37, 4, 40, 41, 8, 9))
  
  
  rs_roster <- rs_stats_j %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  po_roster <- po_stats_j %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  rs_roster_g <- rs_stats_g_j %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  po_roster_g <- po_stats_g_j %>%
    filter(season == cup_season, team == winner) %>%
    select(id, name, team, position)
  
  
  unique_roster <- rbind(rs_roster, po_roster, rs_roster_g, po_roster_g) %>%
    distinct() %>%
    mutate(season = cup_season) 
  
  
  # merge with awards
  unique_roster <- unique_roster %>%
    left_join(select(award_season, fhmID, achievementName), by = c("id" = "fhmID"))

  return(unique_roster)
  
  
  
}

cup_history_list <- list()

for (i in 80:85) {
  temp_df <- challenge_cup_summary(i)
  cup_history_list[[i]] <- temp_df
}

combined_cup_history <- do.call(rbind, cup_history_list)



############################
### Team profile summary ###
############################

# get list of teams HoF players played in
# Only works with FHM era index
# Will not list seasons/teams played for prior to S53 at the moment
hof_by_team <- rs_stats %>%
  filter(id %in% hof_df$playerID) %>%
  group_by(id, team) %>%
  summarise(name = name[1],
            seasons = paste0(season, collapse = ","))
  
  
  

shl_profile_summary <- function(team_id, 
                                season_record_last_updated, 
                                min_draft_season, 
                                current_draft_season) {
  
  team_abbr <- team_abb_map$abbreviation[team_abb_map$id == team_id]
  
  if (length(team_abbr) != 1) {
    stop("Team abbreviation is not length of 1")
  }
  
  team_record <- shl_standings %>%
    left_join(select(team_meta, id, season, abbreviation, conference),
              by = c("id", "season", "abbreviation")) %>%
    group_by(season, conference) %>%
    arrange(season, conference, desc(points), desc(ROW), desc(wins), desc(goalDiff)) %>%
    mutate(conf_rank = row_number()) %>%
    group_by(season) %>%
    arrange(season, desc(points), desc(ROW), desc(wins), desc(goalDiff)) %>%
    mutate(lg_rank = row_number()) %>%
    mutate(conf_suffix = case_when(conf_rank == 1 ~ "1st",
                                   conf_rank == 2 ~ "2nd",
                                   conf_rank == 3 ~ "3rd",
                                   TRUE ~ paste0(conf_rank, "th"))) %>%
    mutate(lg_suffix = case_when(lg_rank == 1 ~ "1st",
                                   lg_rank == 2 ~ "2nd",
                                   lg_rank == 3 ~ "3rd",
                                   TRUE ~ paste0(lg_rank, "th"))) %>%
    filter(id == team_id, season > season_record_last_updated) %>%
    mutate(conference = ifelse(conference == 1, "Western", "Eastern")) %>%
    arrange(season) %>%
    mutate(label = paste0("S", season, ": ", wins, "-", losses, "-", OTL, " (", conf_suffix, " in ", conference, ", ", lg_suffix, " in Leeague)"))
    
  team_record_combined <- team_record %>%
    ungroup() %>%
    summarise(record = paste0(sum(wins), "-", sum(losses), "-", sum(OTL)))
  
  
  
  
  team_hof <- hof_by_team %>%
    filter(team == team_abbr) %>%
    left_join(select(hof_df, playerID, designation, seasonInducted), by = c("id" = "playerID")) %>%
    mutate(label = paste0("S", seasonInducted, " - ", designation, " - ", name, " (", seasons, ")"))
  
  
  team_awards <- team_awards_df %>%
    filter(leagueID == 0, teamID == team_id) %>%
    rename("season" = "seasonID",
           "award" = "achievementName") %>%
    mutate(award = factor(award, levels = c("Presidents Trophy",
                                            "Mathias Chouinard",
                                            "Cole Reinhart",
                                            "Challenge Cup"))) %>%
    arrange(season, award) %>%
    mutate(label = paste0("S", season, ": ", award)) %>%
    select(season, label)
           
   
  player_awards <- player_award_winners %>%
    filter(isAward == TRUE, leagueID == 0, teamID == team_id) %>%
    rename("season" = "seasonID",
           "award" = "achievementName") %>%
    arrange(season) %>%
    mutate(label = paste0("S", season, ": ", award, " - ", playerName))  %>%
    select(season, label)
    
  
  user_awards <- user_awards_df %>%
    filter(leagueID ==0, 
           won == TRUE, 
           isAward == TRUE,
           teamID == team_id) %>%
    rename("season" = "seasonID",
           "award" = "achievementName") %>%
    arrange(season, award, GM) %>%
    group_by(season, award) %>%
    summarise(label = paste0(username, collapse = "/")) %>%
    mutate(label = paste0("S", season, ": ", award, " - ", label)) %>%
    ungroup() %>%
    select(season, label)
  
  
  combined_awards <- rbind(user_awards, player_awards, team_awards) %>%
    arrange(season) %>%
    select(-season)
  
  
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
  
  
  write_lines(combined_awards$label, paste0("Projects/History catchup and code/", team_abbr, "_awards.txt"))
  write_lines(first_round_null$label, paste0("Projects/History catchup and code/", team_abbr, "_draft.txt"))
  write_lines(team_record$label, paste0("Projects/History catchup and code/", team_abbr, "_rs_standings.txt"))
  write_lines(team_record_combined, paste0("Projects/History catchup and code/", team_abbr, "_rs_combined_record.txt"))
  write_lines(team_hof, paste0("Projects/History catchup and code/", team_abbr, "_HoF.txt"))
  
  
  
}
results <- shl_profile_summary(team_id = 1, season_last_updated = 68)
View(results[[1]])
View(results[[2]])
View(results[[3]])
View(results[[4]])
View(results[[5]])
View(results[[6]])
