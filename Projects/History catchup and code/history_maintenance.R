##############
### Notes ####
##############


# Playoff results are hard coded to start at S68 - season when most of that section was stopped

library(tidyverse)
library(httr)
library(jsonlite)
source("scraper_functions.R")


###########################################################
################## Load and prepare data ##################
###########################################################


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


player_award_winners <- player_awards_df %>%
  filter(won == TRUE)

hof_link <- GET("https://portal.simulationhockey.com/api/v1/hof")
hof_df <- fromJSON(rawToChar(hof_link$content))


# Load GM tenure
shl_tenure <- read_csv("Projects/History catchup and code/shl_gm_start.csv")
smjhl_tenure <- read_csv("Projects/History catchup and code/smjhl_gm_start.csv")

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


# Load team all time stats
shl_team_stats_all_time <- read_csv("Projects/History catchup and code/shl_all_time_results.csv")
smjhl_team_stats_all_time <- read_csv("Projects/History catchup and code/smjhl_all_time_results.csv")


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


####################################################################
############### Scrape data for forum posting ######################
####################################################################


#####################
### Draft summary ###
#####################

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
  


########################
### Team cup rosters ###
########################

team_cup_roster <- function(team_abbr, cup_season, league_numeric) {
  
  
  # #testing
  # team_abbr <- "EDM"
  # cup_season <- 86
  # league_numeric <- 0
  
  if (league_numeric == 0) {
    
    rs_stats_fx <- rs_stats
    po_stats_fx <- po_stats
    rs_stats_fx_g <- rs_stats_g
    po_stats_fx_g <- po_stats_g
    ratings_fx <- ratings
    cup_fx <- "challenge"
    
  } else {
    
    rs_stats_fx <- rs_stats_j
    po_stats_fx <- po_stats_j
    rs_stats_fx_g <- rs_stats_g_j
    po_stats_fx_g <- po_stats_g_j  
    ratings_fx <- j_ratings
    cup_fx <- "four_star"
  }
  
  
  rs_roster <- rs_stats_fx %>%
    filter(season == cup_season, team == team_abbr) %>%
    select(id, name, team, position)
  
  po_roster <- po_stats_fx %>%
    filter(season == cup_season, team == team_abbr) %>%
    select(id, name, team, position)
  
  rs_roster_g <- rs_stats_fx_g %>%
    filter(season == cup_season, team == team_abbr) %>%
    select(id, name, team, position)
  
  po_roster_g <- po_stats_fx_g %>%
    filter(season == cup_season, team == team_abbr) %>%
    select(id, name, team, position)
  
  
  unique_roster <- rbind(rs_roster, po_roster, rs_roster_g, po_roster_g) %>%
    distinct() %>%
    mutate(season = cup_season) %>%
    left_join(ratings_fx) %>%
    mutate(pos = case_when(position == "G" ~ "b",
                           TRUE ~ "a")) %>%
    arrange(pos, desc(appliedTPE)) %>%
    mutate(label = paste0(name, " (", position, ")"))
  
  
  write_lines(unique_roster$label, paste0("Projects/History catchup and code/Cup rosters/S", 
                                 cup_season, "_", 
                                 cup_fx, "_", 
                                 team_abbr,
                                 "_roster.txt")
  )
  
}


#######################
### Team cup awards ###
#######################

team_cup_awards <- function(team_abbr, season, league_numeric) {
  
  if (league_numeric == 0) {
    
    award_levels <- c("Challenge Cup",
                      "Presidents Trophy",
                      "Mathias Chouinard",
                      "Cole Reinhart")
    
    all_star_levels <-  c("1st All Star Team", 
                          "2nd All Star Team", 
                          "3rd All Star Team", 
                          "Rookie All Star Team")
    
    stat_df <- rs_stats
    team_meta <- team_meta
    team_numeric <- team_abb_map$id[team_abb_map$abbreviation == team_abbr]
    cup <- "challenge"
    
  } else {
    
    award_levels <- c("Four Star Cup",
                      "Laurifer Trophy",
                      "King-Kurczewski",
                      "Linna-Landvik")
    
    all_star_levels <- c("1st All Star Team", 
                        "2nd All Star Team", 
                        "Defense All Star Team", 
                        "Rookie All Star Team")
    
    stat_df <- rs_stats_j
    team_meta <- team_meta_j
    team_numeric <- team_abb_map_j$id[team_abb_map_j$abbreviation == team_abbr]
    cup <- "four_star"
    
  }
  
  
  # Format team awards
  team_awards <- team_awards_df %>%
    filter(leagueID == league_numeric, teamID == team_numeric, seasonID == season) %>%
    rename("award" = "achievementName") %>%
    mutate(award = factor(award, levels = award_levels)) %>%
    arrange(award) %>%
    mutate(label = award) %>%
    select(label)
  
  
  # Format player awards
  player_awards <- player_award_winners %>%
    filter(isAward == TRUE, leagueID == league_numeric, teamID == team_numeric, seasonID == season) %>%
    rename("award" = "achievementName") %>%
    mutate(label = paste0(award, " - ", playerName))  %>%
    select(label)
  
  # Format user awards
  user_awards <- user_awards_df %>%
    filter(leagueID ==league_numeric, 
           won == TRUE, 
           isAward == TRUE,
           teamID == team_numeric,
           seasonID == season) %>%
    rename("award" = "achievementName") %>%
    arrange(award, GM) %>%
    group_by(award) %>%
    summarise(label = paste0(username, collapse = "/")) %>%
    mutate(label = paste0(award, " - ", label)) %>%
    ungroup() %>%
    select(label)
  
  
  # Merge all awards together
  combined_awards <- rbind(team_awards, user_awards, player_awards) 
  
  
  # Now collect all-star players for each team
  all_stars <- player_awards_df %>%
    filter(seasonID == season,
           leagueID == league_numeric,
           teamID == team_numeric,
           achievementName %in% all_star_levels) %>%
    left_join(select(stat_df, id, pos, season), by = c("fhmID" = "id", "seasonID" = "season")) %>%
    left_join(select(team_meta, id, nameDetails_second, season), by = c("teamID" = "id", "seasonID" = "season")) %>%
    mutate(pos = case_when(is.na(pos) ~ "G",
                           pos == "Forward" ~ "F",
                           pos == "Defense" ~ "D"),
           pos = factor(pos, levels = c("F", "D", "G")),
           
           achievementName = factor(achievementName,
                                    levels = all_star_levels)) %>%
    
    arrange(achievementName, pos) %>%
    
    mutate(team_name = tolower(nameDetails_second)) %>%
    
    mutate(label = paste0(pos, " - ", playerName))
  
  first_as <- all_stars$label[all_stars$achievementName == "1st All Star Team"]
  second_as <- all_stars$label[all_stars$achievementName == "2nd All Star Team"]
  third_as <- all_stars$label[all_stars$achievementName == "3rd All Star Team"]
  fourth_as <- all_stars$label[all_stars$achievementName == "Rookie All Star Team"]
  
  
  # Merge awards and all-stars together
  final_list <- c(
    "awards",
    as.character(combined_awards$label),
    paste0("[b]", gsub("All Star ", "", all_star_levels[1]), ":[/b]"),
    first_as,
    paste0("[b]", gsub("All Star ", "", all_star_levels[2]), ":[/b]"),
    second_as,
    paste0("[b]", gsub("All Star ", "", all_star_levels[3]), ":[/b]"),
    third_as,
    paste0("[b]", gsub("All Star ", "", all_star_levels[4]), ":[/b]"),
    fourth_as
  )
  
  write_lines(final_list, paste0("Projects/History catchup and code/Cup awards/S", 
                                season, "_", 
                                cup, "_", 
                                team_abbr,
                                "_awards.txt")
             )
  
}
  
############################
### Team profile records ###
############################

team_profile_records <- function(season, league_numeric) {
  
  
  # season <- 87
  # league_numeric <- 0
  
  if (league_numeric == 0) {
    
    records <- shl_team_stats_all_time
    gm_sheet <- shl_tenure
    team_names <- team_abb_map
    league_char <- "SHL"
    
  } else {
    
    records <- smjhl_team_stats_all_time
    gm_sheet <- smjhl_tenure
    team_names <- team_abb_map_j
    league_char <- "SMJHL"
    
  }
  
  team_records <- records %>%
    group_by(id) %>%
    summarise(at_wins = sum(wins),
              at_loss = sum(losses),
              at_otl = sum(overtimeLosses) + sum(shootoutLosses),
              at_perc = round(at_wins/(at_wins + at_loss + at_otl), 3)) %>%
    left_join(team_names) %>%
    arrange(abbreviation) %>%
    mutate(label = paste0(abbreviation, ": ",
                          at_wins, "-",
                          at_loss, "-",
                          at_otl, " (",
                          at_perc, "%)"))
  
  team_records_label <- team_records$label
  
  
  gm_records <- records %>%
    left_join(team_names) %>%
    left_join(gm_sheet, by = c("abbreviation" = "team")) %>%
    mutate(gm = ifelse(start == end,
                       paste0(gm, " (need to finish record of previous GM)"),
                       gm)) %>%
    group_by(id) %>%
    filter(season >= start & season <= end) %>%
    summarise(gm = gm[1],
              at_wins = sum(wins),
              at_loss = sum(losses),
              at_otl = sum(overtimeLosses) + sum(shootoutLosses),
              at_perc = round(at_wins/(at_wins + at_loss + at_otl), 3)) %>%
    left_join(team_names) %>%
    arrange(abbreviation) %>%
    mutate(label = paste0(gm, ": ",
                          at_wins, "-",
                          at_loss, "-",
                          at_otl, " (",
                          at_perc, "%)"))
  
  gm_records_label <- gm_records$label
  
  records_output <- c(
    "Overall team records",
    "",
    team_records_label,
    "",
    "",
    "GM records",
    "",
    gm_records_label
  )
  
  write_lines(records_output, paste0("Projects/History catchup and code/Team records/", league_char, "_team_records.txt"))
  
  
}




######################
### Team standings ###
######################

team_profile_standings <- function(season_numeric, league_numeric) {
  
  # season_numeric <- 87
  # league_numeric <- 0
  
  if (league_numeric == 0) {
    
    records <- shl_team_stats_all_time
    standings <- shl_standings
    team_meta <- team_meta
    team_names <- team_abb_map
    league_char <- "SHL"
    po_sch_sum <- po_sch_sum
    
  } else {
    
    records <- smjhl_team_stats_all_time
    standings <- j_standings
    team_meta <- team_meta_j
    team_names <- team_abb_map_j
    league_char <- "SMJHL"
    po_sch_sum <- po_sch_sum_j
    
  }
  
  
  
  # Format team record
  # Season by season and also total record since the last recorded season
  team_standings <- standings %>%
    left_join(select(team_meta, id, season, abbreviation, conference),
              by = c("id", "season", "abbreviation")) %>%
    filter(season == season_numeric) %>%
    group_by(conference) %>%
    arrange(conference, desc(points), desc(ROW), desc(wins), desc(goalDiff)) %>%
    mutate(conf_rank = row_number()) %>%
    ungroup() %>%
    arrange(desc(points), desc(ROW), desc(wins), desc(goalDiff)) %>%
    mutate(lg_rank = row_number()) %>%
    mutate(conf_suffix = case_when(conf_rank == 1 ~ "1st",
                                   conf_rank == 2 ~ "2nd",
                                   conf_rank == 3 ~ "3rd",
                                   TRUE ~ paste0(conf_rank, "th"))) %>%
    mutate(lg_suffix = case_when(lg_rank == 1 ~ "1st",
                                 lg_rank == 2 ~ "2nd",
                                 lg_rank == 3 ~ "3rd",
                                 lg_rank == 21 ~ "21st",
                                 lg_rank == 22 ~ "22nd",
                                 lg_rank == 23 ~ "23rd",
                                 TRUE ~ paste0(lg_rank, "th"))) %>%
    mutate(conference = ifelse(conference == 1, "Western", "Eastern")) %>%
    mutate(label = paste0(abbreviation, ": ", wins, "-", losses, "-", OTL, " (", conf_suffix, " in ", conference, ", ", lg_suffix, " in League)")) %>%
    arrange(abbreviation)
  
  team_standings_label <- team_standings$label
  
  
  
  # Format playoff results
  playoff_results <- po_sch_sum %>%
    filter(season == season_numeric) %>%
    left_join(team_names, by = c("team" = "id")) %>%
    mutate(label = paste0(label))
    
  
  playoffs_null <- team_names %>%
    left_join(select(playoff_results, abbreviation, label)) %>%
    mutate(label = ifelse(is.na(label), "Did not qualify", label)) %>%
    mutate(final_label = paste0(abbreviation, ": ", label))
  
  
  playoffs_null_label <- playoffs_null$final_label
  
  standings_output <- c(
    "Regular season",
    "",
    team_standings_label,
    "",
    "",
    "Playoffs",
    "",
    playoffs_null_label
  )
  
  write_lines(standings_output, paste0("Projects/History catchup and code/Team standings/", league_char, "_team_standings.txt"))
  
  
}

###########################
### Team profile awards ###
###########################

team_profile_awards <- function(league_numeric, season_numeric) {
  
  # season_numeric <- 86
  # league_numeric <- 0
  
  if (league_numeric == 0) {
    
    
    award_levels <- c("Presidents Trophy",
                      "Mathias Chouinard",
                      "Cole Reinhart",
                      "Challenge Cup")
    team_meta <- team_meta
    team_names <- team_abb_map
    league_char <- "SHL"
    
    
  } else {
    
    
    award_levels <- c("Laurifer Trophy",
                      "King-Kurczewski",
                      "Linna-Landvik",
                      "Four Star Cup")
    team_meta <- team_meta_j
    team_names <- team_abb_map_j
    league_char <- "SMJHL"
    
  }
  
  
  # Format and merge all team awards
  team_awards <- team_awards_df %>%
    left_join(team_names, by = c("teamID" = "id")) %>%
    filter(leagueID == league_numeric, 
           seasonID == season_numeric) %>%
    rename("award" = "achievementName") %>%
    mutate(award = factor(award, levels = award_levels)) %>%
    arrange(abbreviation, award) %>%
    mutate(label = award) %>%
    select(abbreviation, label)
  
  
  player_awards <- player_award_winners %>%
    left_join(team_names, by = c("teamID" = "id")) %>%
    filter(isAward == TRUE, 
           leagueID == league_numeric, 
           seasonID == season_numeric) %>%
    rename("award" = "achievementName") %>%
    arrange(abbreviation) %>%
    mutate(label = paste0(award, " - ", playerName))  %>%
    select(abbreviation, label)
  
  
  user_awards <- user_awards_df %>%
    left_join(team_names, by = c("teamID" = "id")) %>%
    filter(leagueID == league_numeric, 
           won == TRUE, 
           isAward == TRUE,
           seasonID == season_numeric) %>%
    rename("award" = "achievementName") %>%
    arrange(abbreviation, award, GM) %>%
    group_by(abbreviation, award) %>%
    summarise(label = paste0(username, collapse = "/")) %>%
    mutate(label = paste0(award, " - ", label)) %>%
    ungroup() %>%
    select(abbreviation, label)
  
  
  combined_awards <- rbind(user_awards, player_awards, team_awards) %>%
    arrange(abbreviation) %>%
    mutate(label_with_team = paste0(abbreviation, ": ", label))
  
  write_lines(standings_output, paste0("Projects/History catchup and code/Team awards/", combined_awards$label, "_team_awards.txt"))
  write_lines(standings_output, paste0("Projects/History catchup and code/Team awards/", combined_awards$label_with_team, "_team_awards_with_team.txt"))
  
  
}


#########################
### Team summary page ###
#########################

team_summary_page <- function(league_numeric, season_numeric) {
  
  
  pres <- team_awards_df %>%
    filter(achievementName == "Presidents Trophy") %>%
    select(teamID, achievementName) %>%
    group_by(teamID) %>%
    summarise(n = n())
  
  cup <- team_awards_df %>%
    filter(achievementName == "Challenge Cup") %>%
    select(teamID, achievementName) %>%
    group_by(teamID) %>%
    summarise(n = n())
  
  reg_season <- h1 %>%
    mutate(p_perc = points/(2*gamesPlayed)) %>%
    mutate(otl = overtimeLosses + shootoutLosses) %>%
    select(id, name, nickname, wins, losses, otl, points, p_perc) %>%
    mutate(p_perc = round(p_perc, 3)) %>%
    left_join(pres, by = c("id" = "teamID"))
  
  
  
  
  po <- h2 %>%
    mutate(playoffResult_sum = case_when(playoffResult == "Missed" ~ "Missed",
                                         TRUE ~ "Made")) %>%
    group_by(id) %>%
    summarise(name = paste0(first(name), " ", first(nickname)),
              wins = sum(playoffWins, na.rm = T),
              losses = sum(playoffLosses, na.rm = T),
              made = sum(playoffResult_sum == "Made"),
              missed = sum(playoffResult_sum == "Missed")) %>%
    left_join(cup, by = c("id" = "teamID"))
  
  
  
  h2 %>%
    mutate(yaxis = case_when(playoffResult == "Missed" ~ -1,
                             TRUE ~ 1)) %>%
    group_by(id) %>%
    mutate(name = paste0(first(name), " ", first(nickname))) %>%
    ggplot(aes(x = season, y = yaxis, fill = factor(yaxis))) +
    geom_col(show.legend = F,
             col = "black",
             width = .75) +
    facet_wrap(.~name) 
  
  
  
  
  
  streaks <- h2 %>%
    arrange(season) %>%
    mutate(streak_stat = ifelse(playoffResult != "Missed", 0, 1)) %>%
    group_by(id) %>%
    mutate(name = paste0(last(name), " ", last(nickname))) %>%
    mutate(steak_id = cumsum(c(1, diff(streak_stat) != 0))) %>% 
    ungroup()
  
  stat_streaks <- streaks %>%
    filter(streak_stat == 1) %>%
    group_by(id, steak_id) %>%
    summarise(
      season_start = min(season),
      season_end = max(season),
      team = paste0(unique(name), collapse = ","),
      streak_length = n()
      
    ) %>%
    ungroup() %>%
    arrange(desc(streak_length)) %>%
    mutate(rank = row_number()) %>%
    group_by(id) %>%
    filter(streak_length == max(streak_length))
  
}


#################
### All stars ###
#################


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
                                  "Defense All Star Team", 
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
                                               "Defense All Star Team", 
                                               "Rookie All Star Team"))) %>%
    
    arrange(achievementName, pos) %>%
    
    mutate(team_name = tolower(nameDetails_second)) %>%
    
    mutate(label = paste0(pos, " - ", playerName, " :", team_name, ":"))
  
  first_as <- all_stars$label[all_stars$achievementName == "1st All Star Team"]
  second_as <- all_stars$label[all_stars$achievementName == "2nd All Star Team"]
  def_as <- all_stars$label[all_stars$achievementName == "Defense All Star Team"]
  rookie_as <- all_stars$label[all_stars$achievementName == "Rookie All Star Team"]
  
  file <- c(
    "[align=center]",
    paste0("[size=x-large][b][u]S", season, " All-Star Teams[/u][/b][/size]"),
    "",
    "",
    "[size=large][b]1st Team All-SHL[/b][/size]",
    first_as,
    "",
    "[size=large][b]2nd Team All-SHL[/b][/size]",
    second_as,
    "",
    "[size=large][b]Defense Team All-SHL[/b][/size]",
    def_as,
    "",
    "[size=large][b]Rookie Team All-SHL[/b][/size]",
    rookie_as,
    "[/align]"
  )
  
  write_lines(file, paste0("Projects/History catchup and code/all_stars/s", season, "_smjhl_all_stars.txt"))
}

