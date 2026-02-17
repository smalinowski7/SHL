##############
### Notes ####
##############

# SHL team profile function
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
    mutate(season = cup_season) %>%
    left_join(ratings) %>%
    mutate(pos = case_when(position == "G" ~ "b",
                           TRUE ~ "a")) %>%
    arrange(pos, desc(appliedTPE)) %>%
    mutate(label = paste0(name, " (", position, ")"))
  
  
  # merge with awards
  awards_hof <- unique_roster %>%
    left_join(select(award_season, fhmID, achievementName, achievementDescription), by = c("id" = "fhmID")) %>%
    left_join(select(hof_df, playerID, seasonInducted), by = c("id" = "playerID")) %>%
    filter(!is.na(achievementName) | !is.na(seasonInducted)) 
    
  write_lines(unique_roster$label, paste0("Projects/History catchup and code/s", cup_season, "_challenge_cup.txt"))
  write_csv(awards_hof, paste0("Projects/History catchup and code/s", cup_season, "_award_hof_list.csv"))
  
  
  
  
}




four_star_summary <- function(cup_season) {
  
  
  # Filter for winner
  winner <- four_star_cup %>%
    distinct() %>%
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
    mutate(season = cup_season) %>%
    left_join(j_ratings) %>%
    mutate(pos = case_when(position == "G" ~ "b",
                           TRUE ~ "a")) %>%
    arrange(pos, desc(appliedTPE), name) %>%
    mutate(label = paste0(name, " (", position, ")"))
  
  
  # merge with awards
  awards_hof <- unique_roster %>%
    left_join(select(award_season, fhmID, achievementName, achievementDescription), by = c("id" = "fhmID")) %>%
    filter(!is.na(achievementName))
    
  write_lines(unique_roster$label, paste0("Projects/History catchup and code/s", cup_season, "_4star_cup.txt"))
  write_csv(awards_hof, paste0("Projects/History catchup and code/s", cup_season, "_smjhl_award_list.csv"))
  
  
  # merge with awards
  unique_roster <- unique_roster %>%
    left_join(select(award_season, fhmID, achievementName), by = c("id" = "fhmID"))

  return(unique_roster)
  
  
  
}



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
                                current_draft_season,
                                min_playoff_season,
                                max_playoff_season) {
  
  team_abbr <- team_abb_map$abbreviation[team_abb_map$id == team_id]
  
  if (length(team_abbr) != 1) {
    stop("Team abbreviation is not length of 1")
  }
  
  # Format team record
  # Season by season and also total record since the last recorded season
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
    mutate(label = paste0("S", season, ": ", wins, "-", losses, "-", OTL, " (", conf_suffix, " in ", conference, ", ", lg_suffix, " in League)"))
    
  team_record_combined <- team_record %>%
    ungroup() %>%
    summarise(record = paste0(sum(wins), "-", sum(losses), "-", sum(OTL)))
  
  
  
  # Format playoff results
  playoff_results <- po_sch_sum %>%
    filter(team == team_id)
  
  playoffs_null <- data.frame(season = min_playoff_season:max_playoff_season) %>%
    left_join(select(playoff_results, season, label)) %>%
    mutate(label = ifelse(is.na(label), "Did not qualify", label)) %>%
    mutate(final_label = paste0("S", season, ": ", label))
  
  # Collect team HoF players
  # Will have to do manually for HoF GMs later
  team_hof <- hof_by_team %>%
    filter(team == team_abbr) %>%
    left_join(select(hof_df, playerID, designation, seasonInducted), by = c("id" = "playerID")) %>%
    mutate(label = paste0("S", seasonInducted, " - ", designation, " - ", name, " (", seasons, ")"))
  
  
  
  # Format and merge all team awards
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
  
  
  
  
  write_lines(combined_awards$label, paste0("Projects/History catchup and code/", team_abbr, "_awards.txt"))
  write_lines(first_round_null$label, paste0("Projects/History catchup and code/", team_abbr, "_draft.txt"))
  write_lines(team_record$label, paste0("Projects/History catchup and code/", team_abbr, "_rs_standings.txt"))
  write_lines(team_record_combined, paste0("Projects/History catchup and code/", team_abbr, "_rs_combined_record.txt"))
  write_lines(team_hof$label, paste0("Projects/History catchup and code/", team_abbr, "_HoF.txt"))
  write_lines(playoffs_null$final_label, paste0("Projects/History catchup and code/", team_abbr, "_playoff_results.txt"))
  
  
  
}





smjhl_profile_summary <- function(team_id, 
                                season_record_last_updated, 
                                min_draft_season, 
                                current_draft_season,
                                min_playoff_season,
                                max_playoff_season) {
  
  team_abbr <- team_abb_map_j$abbreviation[team_abb_map_j$id == team_id]
  
  if (length(team_abbr) != 1) {
    stop("Team abbreviation is not length of 1")
  }
  
  # Format team record
  # Season by season and also total record since the last recorded season
  team_record <- j_standings %>%
    left_join(select(team_meta_j, id, season, abbreviation, conference),
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
    mutate(conference = ifelse(conference == 1, "Southern", "Northern")) %>%
    arrange(season) %>%
    mutate(label = paste0("S", season, ": ", wins, "-", losses, "-", OTL, " (", conf_suffix, " in ", conference, ", ", lg_suffix, " in League)"))
  
  team_record_combined <- team_record %>%
    ungroup() %>%
    summarise(record = paste0(sum(wins), "-", sum(losses), "-", sum(OTL)))
  
  
  
  # Format playoff results
  playoff_results <- po_sch_sum_j %>%
    filter(team == team_id)
  
  playoffs_null <- data.frame(season = min_playoff_season:max_playoff_season) %>%
    left_join(select(playoff_results, season, label)) %>%
    mutate(label = ifelse(is.na(label), "Did not qualify", label)) %>%
    mutate(final_label = paste0("S", season, ": ", label))
  

  
  # Format and merge all team awards
  team_awards <- team_awards_df %>%
    filter(leagueID == 1, teamID == team_id) %>%
    rename("season" = "seasonID",
           "award" = "achievementName") %>%
    mutate(award = factor(award, levels = c("Laurifer Trophy",
                                            "King-Kurczewski",
                                            "Linna-Landvik",
                                            "Four Star Cup"))) %>%
    arrange(season, award) %>%
    mutate(label = paste0("S", season, ": ", award)) %>%
    select(season, label)
  
  
  player_awards <- player_award_winners %>%
    filter(isAward == TRUE, leagueID == 1, teamID == team_id) %>%
    rename("season" = "seasonID",
           "award" = "achievementName") %>%
    arrange(season) %>%
    mutate(label = paste0("S", season, ": ", award, " - ", playerName))  %>%
    select(season, label)
  
  
  user_awards <- user_awards_df %>%
    filter(leagueID == 1, 
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
  
  
  
  # Format team first round picks
  first_rounders <- j_first_round %>%
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
  write_lines(playoffs_null$final_label, paste0("Projects/History catchup and code/", team_abbr, "_playoff_results.txt"))
  
  
  
}



w_perc <- function(w,l,otl,sol) {
  
  gp = w+l+otl+sol
  total_l <- otl + sol
  total_record <- paste0(w, "-", l, "-", total_l)
  w_perc <- w/gp
  
  return(list(w_perc, total_record))
}


range_records <- function(seasons, team = NULL) {
  
  team_records <- shl_standings %>%
  filter(season >= min(seasons), season <= max(seasons)) %>%
  group_by(id) %>%
  summarise(abbreviation = last(abbreviation),
            wins = sum(wins),
            losses = sum(losses),
            otl = sum(OTL),
            w_perc = wins/(wins+losses+otl))
  
  
  if (!is.null(team)) {
    team_records <- filter(team_records, abbreviation == team)
  }
  
  return(team_records)
}

shl_profile_summary(team_id = 6, 
                    season_record_last_updated = 68, 
                    min_playoff_season = 68, 
                    max_playoff_season = 85,
                    min_draft_season = 1, 
                    current_draft_season = 86)


hof_fix <-
  hof_by_team %>%
  
  filter(team == "DGG") %>%
  
  left_join(select(hof_df, playerID, designation, seasonInducted), by = c("id" = "playerID")) %>%
  mutate(label = paste0("S", seasonInducted, " - ", designation, " - ", name, " (", seasons, ")")) %>%
  arrange(team, seasonInducted)
  
write_lines(hof_fix$label, "Projects/History catchup and code/all_hof.txt")
                    





shl_all_stars <- function(season) {
  
  all_stars <- player_awards_df %>%
    filter(seasonID == season) %>%
    filter(leagueID == 0,
           achievementName %in% c("1st All Star Team", 
                                  "2nd All Star Team", 
                                  "3rd All Star Team", 
                                  "Rookie All Star Team")) %>%
    left_join(select(rs_stats, id, pos, season), by = c("fhmID" = "id", "seasonID" = "season")) %>%
    left_join(select(team_meta, id, nameDetails_second, season), by = c("teamID" = "id", "seasonID" = "season")) %>%
    mutate(pos = case_when(is.na(pos) ~ "G",
                           pos == "Forward" ~ "F",
                           pos == "Defense" ~ "D"),
           pos = factor(pos, levels = c("F", "D", "G")),
           
           achievementName = factor(achievementName,
                                    levels = c("1st All Star Team", 
                                               "2nd All Star Team", 
                                               "3rd All Star Team", 
                                               "Rookie All Star Team"))) %>%
    
    arrange(achievementName, pos) %>%
    
    mutate(team_name = tolower(nameDetails_second)) %>%
    
    mutate(label = paste0(pos, " - ", playerName, " :", team_name, ":"))
  
  first_as <- all_stars$label[all_stars$achievementName == "1st All Star Team"]
  second_as <- all_stars$label[all_stars$achievementName == "2nd All Star Team"]
  third_as <- all_stars$label[all_stars$achievementName == "3rd All Star Team"]
  rookie_as <- all_stars$label[all_stars$achievementName == "Rookie All Star Team"]
  
  file <- c(
    "[align=center]",
    ":shl: :shl: :shl: :shl: :shl: :shl: :shl:",
    paste0("[size=x-large][b][u]S", season, " All-Star Teams[/u][/b][/size]"),
    ":shl: :shl: :shl: :shl: :shl: :shl: :shl:",
    "",
    "",
    "[size=large][b]1st Team All-SHL[/b][/size]",
    first_as,
    "",
    "[size=large][b]2nd Team All-SHL[/b][/size]",
    second_as,
    "",
    "[size=large][b]3rd Team All-SHL[/b][/size]",
    third_as,
    "",
    "[size=large][b]Rookie Team All-SHL[/b][/size]",
    rookie_as,
    "[/align]"
  )
  
  write_lines(file, paste0("Projects/History catchup and code/all_stars/s", season, "_shl_all_stars.txt"))
}
  


smjhl_all_stars <- function(season) {
  
  all_stars <- player_awards_df %>%
    filter(seasonID == season) %>%
    filter(leagueID == 1,
           achievementName %in% c("1st All Star Team", 
                                  "2nd All Star Team", 
                                  "Defensive All Star Team", 
                                  "Rookie All Star Team")) %>%
    left_join(select(rs_stats_j, id, pos, season), by = c("fhmID" = "id", "seasonID" = "season")) %>%
    left_join(select(team_meta_j, id, nameDetails_second, season), by = c("teamID" = "id", "seasonID" = "season")) %>%
    mutate(pos = case_when(is.na(pos) ~ "G",
                           pos == "Forward" ~ "F",
                           pos == "Defense" ~ "D"),
           pos = factor(pos, levels = c("F", "D", "G")),
           
           achievementName = factor(achievementName,
                                    levels = c("1st All Star Team", 
                                               "2nd All Star Team", 
                                               "Defensive All Star Team", 
                                               "Rookie All Star Team"))) %>%
    
    arrange(achievementName, pos) %>%
    
    mutate(team_name = tolower(nameDetails_second)) %>%
    
    mutate(label = paste0(pos, " - ", playerName, " :", team_name, ":"))
  
  first_as <- all_stars$label[all_stars$achievementName == "1st All Star Team"]
  second_as <- all_stars$label[all_stars$achievementName == "2nd All Star Team"]
  def_as <- all_stars$label[all_stars$achievementName == "Defensive All Star Team"]
  rookie_as <- all_stars$label[all_stars$achievementName == "Rookie All Star Team"]
  
  file <- c(
    "[align=center]",
    ":smjhl: :smjhl: :smjhl: :smjhl: :smjhl: :smjhl: :smjhl:",
    paste0("[size=x-large][b][u]S", season, " All-Star Teams[/u][/b][/size]"),
    ":smjhl: :smjhl: :smjhl: :smjhl: :smjhl: :smjhl: :smjhl:",
    "",
    "",
    "[size=large][b]1st Team All-SHL[/b][/size]",
    first_as,
    "",
    "[size=large][b]2nd Team All-SHL[/b][/size]",
    second_as,
    "",
    "[size=large][b]Defensive Team All-SHL[/b][/size]",
    def_as,
    "",
    "[size=large][b]Rookie Team All-SHL[/b][/size]",
    rookie_as,
    "[/align]"
  )
  
  write_lines(file, paste0("Projects/History catchup and code/all_stars/s", season, "_smjhl_all_stars.txt"))
}
