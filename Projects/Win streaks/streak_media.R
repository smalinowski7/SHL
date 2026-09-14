source("scraper_functions.R")
library(tidyverse)

myrleid <- function(x) {
  rl <- rle(x)$lengths
  rep(seq_along(rl), times = rl)
}



# Load the team info and the schedule
meta <- read_csv("Data/SHL/index_team_meta.csv")
compiled_schedule <- read_csv("Data/SHL/index_schedule.csv")


#add the home/away team information to the compiled schedule
compiled_schedule_annotated <- compiled_schedule %>%
  left_join(select(meta, id, abbreviation, season), 
            by = c("awayTeam" = "id", "season")) %>%
  rename("away.team" = abbreviation) %>%
  
  left_join(select(meta, id, abbreviation, season), 
            by = c("homeTeam" = "id", "season")) %>%
  rename("home.team" = abbreviation) %>%
  
  #format the date to be an actual date and sort from oldest to newest
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  arrange(date)


#format the schedule to a tidy version
schedule_list_tidy <- list()
for (teams in meta$abbreviation) {
  
  #filter out each team from the schedule to create an individual dataframe
  temp_schedule <- compiled_schedule_annotated %>%
    filter(away.team == teams | home.team == teams) %>%
    mutate(team = teams,
           opponent = case_when(away.team == teams ~ home.team,
                                home.team == teams ~ away.team))
  schedule_list_tidy[[teams]] <- temp_schedule
}
compiled_schedule_annotated_tidy <- do.call(rbind, schedule_list_tidy)



#format the OT column
formatted_schedule <- compiled_schedule_annotated_tidy %>%
  mutate(OT = case_when(overtime == "1" ~ TRUE,
                        shootout == "1" ~ TRUE,
                        TRUE ~ FALSE),
         Points = case_when(team == away.team & awayScore > homeScore ~ 2,
                            team == away.team & awayScore < homeScore & OT == TRUE ~ 1,
                            team == home.team & homeScore > awayScore ~ 2,
                            team == home.team & homeScore < awayScore & OT == TRUE ~ 1,
                            TRUE ~ 0)) %>%
  group_by(team) %>%
  mutate(gamenum = as.numeric(row_number()),
         cumulative_points = cumsum(Points)) %>% 
  mutate(score_differential = case_when(team == away.team ~ awayScore - homeScore,
                                        team == home.team ~ homeScore - awayScore))





### Find team win streaks
streaks <- formatted_schedule %>%
  mutate(win = ifelse(score_differential > 0, TRUE, FALSE)) %>%
  group_by(team, season) %>%
  mutate(win = win*1) %>% 
  mutate(steak_id = cumsum(c(1, diff(win) != 0))) %>% 
  ungroup()

win_streaks <- streaks %>%
  filter(win == 1) %>%
  group_by(team, steak_id, season) %>%
  summarise(

    streak_length = n()

  ) %>%
  ungroup()         
         

streak_plot <- function(streak_data, team_filter, season_filter) {
  

  streak_data <- streak_data %>%
    filter(team == team_filter & season == season_filter) %>%
    mutate(date2 = date) %>%
    separate(date2, into = c("year", "extra"))
  
  year <- min(streak_data$year)
  year2 <- max(streak_data$year)
  
  
  all_dates <- data.frame(date = seq(as.Date(paste0(year, "-10-01")), as.Date(paste0(year2, "-03-31")), by = "days"))
  merged <- all_dates %>%
    left_join(streak_data) %>%
    mutate(score_differential = ifelse(is.na(score_differential), 0, score_differential))
  
  data <- merged$score_differential
  
  calendR(year,  special.days = data, gradient = TRUE,
          
          legend.pos = "none",
          legend.title = "Title",
          from = paste0(year, "-10-01"), to = paste0(year2, "-03-31"),
          ncol = 3) +
    scale_fill_gradient2(low = "red3", high = "springgreen4", mid = "white", midpoint = 0) +
    labs(title = paste0("S", season_filter, " ", team_filter))
  ggsave(paste0("C://Users/Seth/Desktop/streaks/", season_filter, team_filter, ".png"), width =7, height = 5.5, dpi = 300)
         
}



streak_plot(streaks, "Winnipeg Aurora", 68)
streak_plot(streaks, "Winnipeg Aurora", 70)
streak_plot(streaks, "Hamilton Steelhawks", 57)
streak_plot(streaks, "Texas Renegades", 58)
streak_plot(streaks, "Chicago Syndicate", 84)
streak_plot(streaks, "Atlanta Inferno", 83)
streak_plot(streaks, "Chicago Syndicate", 59)
streak_plot(streaks, "Chicago Syndicate", 64)
streak_plot(streaks, "Edmonton Blizzard", 84)
streak_plot(streaks, "Chicago Syndicate", 62)
streak_plot(streaks, "Winnipeg Aurora", 67)



### only 5 seasons worth of boxscores to just dl them manually
skater_list <- list()
goalie_list <- list()
meta_list <- list()
for (i in c(67, 68, 70, 83, 84)) {
  print(i)
  skater_bs <- read.csv(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/boxscore_skater_summary.csv"), sep = ";")
  goalie_bs <- read.csv(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/boxscore_goalie_summary.csv"), sep = ";")
  
  goalie_bs <- goalie_bs %>%
    filter(TOI > 0) %>%
    mutate(league_sa = sum(SA),
           league_save = sum(SV),
           league_sv_pct = league_save/league_sa,
           league_sht_pct = 1-league_sv_pct) %>%
    mutate(gsea = (league_sht_pct*SA) - GA)
  
  meta <- read.csv(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/player_master.csv"), sep = ";")
  meta_list[[i]] <- meta
  
  skater_list[[i]] <- skater_bs
  goalie_list[[i]] <- goalie_bs
  
}

skater_stats <- do.call(bind_rows, skater_list)
goalie_stats <- do.call(rbind, goalie_list)
meta <- do.call(bind_rows, meta_list)



streak_stats <- function(sch, skater_data, goalie_data, season_filter, streak_id, team_id, player_meta) {
  
  # sch <- streaks
  # skater_data <- skater_stats
  # goalie_data <- goalie_stats
  # season_filter <- 62
  # streak_id <- 5
  # team_id <- 1
  # player_meta <- meta
  
  
  team_name <- team_meta$name[team_meta$id == team_id]
    
  game_ids <- sch %>%
    filter(season == season_filter & steak_id == streak_id & team == team_name)
  
  team_stats <- game_ids %>%
    mutate(gf = ifelse(team == home.team, homeScore, awayScore),
           ga = ifelse(team == home.team, awayScore, homeScore)) %>%
    group_by(team) %>%
    summarise(gf = sum(gf),
              ga = sum(ga),
              gf_g = gf/n(),
              ga_g = ga/n(),
              diff = sum(score_differential),
              diff_game = diff/n())
  
  if (season_filter >= 66) {
    game_ids <- game_ids$gameid
    
    streak_skater_bs <- skater_data %>%
      filter(Game.Id %in% game_ids) %>%
      filter(TeamId == team_id) %>%
      group_by(PlayerId) %>%
      summarise(across(c(G, A, SOG, BS, HT, TK), sum)) %>%
      left_join(select(player_meta, "PlayerId", "Last.Name")) %>%
      mutate(P = G+A) %>%
      distinct() %>%
      arrange(desc(P)) %>%
      
      select(Last.Name, G, A, P, SOG, BS, HT, TK)
    
    
    
    streak_goalie_stats <- goalie_data %>%
      filter(Game.Id %in% game_ids) %>%
      filter(TeamId == team_id) %>%
      group_by(PlayerId) %>%
      summarise(save_pct = sum(SV)/sum(SA),
                gsaa = sum(gsea)) %>%
      left_join(select(meta, PlayerId, Last.Name)) %>%
      distinct() %>%
      select(Last.Name, save_pct, gsaa)
    
    
    return(list(team_stats, streak_skater_bs, streak_goalie_stats))
  } else {
    return(team_stats)
  }
  
    
}


streak_stats(streaks, skater_stats, goalie_stats, 68, 7, 11, meta)
streak_stats(streaks, skater_stats, goalie_stats, 70, 23, 11, meta)
streak_stats(streaks, skater_stats, goalie_stats, 57, 1, 2, meta)
streak_stats(streaks, skater_stats, goalie_stats, 58, 9, 15, meta)
streak_stats(streaks, skater_stats, goalie_stats, 84, 21, 1, meta)
streak_stats(streaks, skater_stats, goalie_stats, 83, 3, 18, meta)
streak_stats(streaks, skater_stats, goalie_stats, 59, 1, 1, meta)
streak_stats(streaks, skater_stats, goalie_stats, 64, 19, 1, meta)
streak_stats(streaks, skater_stats, goalie_stats, 84, 7, 9, meta)
streak_stats(streaks, skater_stats, goalie_stats, 62, 5, 1, meta)
streak_stats(streaks, skater_stats, goalie_stats, 67, 23, 11, meta)



boxscore <- read.csv("https://simulationhockey.com/games/shl/S84/csv/boxscore_skater_summary.csv", sep = ";")
meta <- read.csv("https://simulationhockey.com/games/shl/S84/csv/player_master.csv", sep = ";")
goalie <- read.csv("https://simulationhockey.com/games/shl/S84/csv/boxscore_goalie_summary.csv", sep = ";")

goalie_stats <- goalie %>%
  filter(TOI > 0) %>%
  mutate(league_sa = sum(SA),
         league_save = sum(SV),
         league_sv_pct = league_save/league_sa,
         league_sht_pct = 1-league_sv_pct) %>%
  mutate(gsea = (league_sht_pct*SA) - GA) %>%
  filter(Game.Id >= low & Game.Id <= high) %>%
  filter(TeamId == 9) %>%
  group_by(PlayerId) %>%
  summarise(save_pct = sum(SV)/sum(SA),
            gsaa = sum(gsea)) %>%
  left_join(select(meta, PlayerId, Last.Name)) %>%
  select(Last.Name, save_pct, gsaa)

edm_boxscore <- boxscore %>% filter(TeamId == 9) %>%
  filter(Game.Id >= low & Game.Id <= high)

stats <- edm_boxscore %>%
  group_by(PlayerId) %>%
  summarise(across(c(G, A, SOG, BS, HT, TK), sum)) %>%
  left_join(meta) %>%
  mutate(P = G+A) %>%
  
  select(Last.Name, G, A, P, SOG, BS, HT, TK)
