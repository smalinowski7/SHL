library(tidyverse)
library(httr)
library(jsonlite)
library(gganimate)
library(gifski)


last <- function(x) { return( x[length(x)] ) }



###################################
### Data loading and formatting ###
###################################


seasons <- c(66:80)



team_meta <- GET("https://index.simulationhockey.com/api/v1/teams")
team_meta <- fromJSON(rawToChar(team_meta$content))
team_meta_merge <- team_meta %>% select(id, abbreviation)
team_colors <- team_meta$colors$primary
names(team_colors) <- team_meta$abbreviation
team_color_abbr <- team_meta$colors$primary
names(team_color_abbr) <- team_meta$id


#Scrape the schedule and merge for all seasons in the league
schedule_list <- list()
for (i in seasons) {
  print(i)
  schedule_url <-GET("http://index.simulationhockey.com/api/v1/schedule", query = list(season = i))
  schedule_url <- fromJSON(rawToChar(schedule_url$content))
  schedule_list[[i]] <- schedule_url
}

schedule <- do.call(rbind, schedule_list)



shootout_shutout <- schedule %>%
  filter(shootout == 1 & homeScore + awayScore == 1) %>%
  select(gameid, homeTeam, awayTeam, season) %>%
  mutate(season = as.character(season)) %>%
  rename("Game.Id" = "gameid")

teams_only <- schedule %>% select(gameid, homeTeam, awayTeam)




#get a list of player stats
# to merge names to IDs and for data validation
player_list <- list()
for (i in seasons) {
  player_stats <- GET("http://index.simulationhockey.com/api/v1/players/stats", query = list(season = i))
  player_stats <- fromJSON(rawToChar(player_stats$content))
  player_stats <- do.call(data.frame, player_stats)
  player_list[[i]] <- player_stats
}
combined_player_stats <- do.call(rbind, player_list)

#get just their names and IDs for merging
player_id_map <- combined_player_stats %>%
  select(name, id) %>%
  group_by(id) %>%
  summarise(name = last(name))

#get a list of career games for players
career_gp <- combined_player_stats %>%
  select(id, gamesPlayed) %>%
  group_by(id) %>%
  summarise(gp = sum(gamesPlayed)) %>%
  ungroup()

#also get a list of all goal scorers to make sure number of unqiue player IDs matches
goalscorers <- combined_player_stats %>%
  filter(goals > 0)



### Get the boxscores
boxscore_directory <- "C://Users/Seth/Desktop/clutch media/"
subfolders <- list.files(boxscore_directory)
subfolders <- subfolders[!subfolders == "Graphs"]


boxscore_list <- list()
for (i in subfolders) {
  temp_directory <- paste0(boxscore_directory, i)
  temp_boxscore <- read.csv(paste0(temp_directory, "/boxscore_period_scoring_summary.csv"),  
                            sep = ";")
  temp_boxscore$season <- i
  boxscore_list[[i]] <- temp_boxscore
}

combined_boxscores_all <- do.call(rbind, boxscore_list)


#filter for regular season only by checking for game IDs in the merged schedule fule
combined_boxscores <- combined_boxscores_all %>%
  filter(Game.Id %in% schedule$gameid)



# start to format for the clutch goal IDs
formatted <- combined_boxscores %>%
  left_join(teams_only, by = c("Game.Id" = "gameid")) %>%
  bind_rows(shootout_shutout) %>%
  arrange(Game.Id, Period, Time) %>%
  group_by(Game.Id) %>%
  mutate(home_score = 1*(TeamId == homeTeam),
         away_score = 1*(TeamId == awayTeam),
         home_score = cumsum(home_score),
         away_score = cumsum(away_score)) %>%
  mutate(score = paste0(away_score, "-", home_score),
         score = case_when(score == "NA-NA" ~ "0-0",
                           TRUE ~ score)) %>%
  ungroup() %>%
  
  mutate(last_home_score = case_when(TeamId == homeTeam ~ home_score - 1,
                                     TeamId == awayTeam ~ home_score),
         last_away_score = case_when(TeamId == awayTeam ~ away_score - 1,
                                     TeamId == homeTeam ~ away_score)) %>%
  
  #create unique event id
  mutate(event_id = row_number())


#make sure our number of unique goal scorers matches
length(unique(goalscorers$id)) == length(unique(formatted$Scorer))

#make sure our dataframes show the same number of games
length(unique(schedule$gameid)) == length(unique(formatted$Game.Id))


formatted <- formatted %>%
  left_join(player_id_map, by = c("Scorer" = "id"))







##################################################
### Player level clutch goals and career stats ###
##################################################



# Find clutch goals
# Game tying or winning goals in the final 3 minutes or overtime
formatted_with_clutch <- formatted %>%
  filter(Period == "3" & Time >= 1020 | Period == "OT1") %>%
  
  mutate(clutch = case_when(
    TeamId == homeTeam & ((last_home_score - last_away_score) %in% c(0,-1)) ~ TRUE,
    TeamId == awayTeam & ((last_away_score - last_home_score) %in% c(0,-1)) ~ TRUE,
    TRUE ~ FALSE
  ))


# filter for only clutch goals
clutch <- filter(formatted_with_clutch, clutch == TRUE)
clutch %>% group_by(Scorer) %>% summarise(n = n(), name = name[1]) %>% View()



# classify goals as game tying vs. game winning
clutch <- formatted_with_clutch %>%
  filter(clutch == TRUE) %>%
  mutate(situation = case_when(TeamId == homeTeam & (home_score-away_score == 0) ~ "Tying goal",
                               TeamId == awayTeam & (home_score - away_score == 0) ~ "Tying goal",
                               TeamId == homeTeam & (home_score - away_score == 1) ~ "Winning goal",
                               TeamId == awayTeam & (away_score - home_score == 1) ~ "Winning goal"))


#calculate player career stats and format data for graphing        
clutch_player_career <- clutch %>%
  left_join(career_gp, by = c("Scorer" = "id")) %>%
  group_by(Scorer, situation) %>%
  arrange(desc(season)) %>%
  summarise(n = n(), 
            name = name[1],
            gp = gp[1],
            n_per_game = n/gp) %>%
  group_by(Scorer) %>%
  mutate(sum = sum(n),
         per_game = sum(n_per_game),
         name = name[1]) %>%
  ungroup() %>%
  arrange(desc(sum)) 


clutch_graph <- clutch_player_career %>%
  filter(sum >= 7) %>%
  mutate(name = factor(name, levels = unique(name)))


#graph
ggplot(clutch_graph, aes(x = n, y = fct_rev(name), fill = situation)) +
  geom_col(col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = "top") +
  scale_x_continuous(expand = c(0.01,0), breaks = 1:13) +
  scale_fill_manual(values = c("#f1a226", "#298c8c")) +
  labs(x = "Career clutch goals", y = NULL, fill = NULL)
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/career_goals.jpg", width = 7, height = 9, dpi = 600)




clutch_graph_per_game <- clutch_player_career %>%
  filter(gp > 200) %>%
  filter(per_game > 0.015) %>%
  arrange(per_game) %>%
  mutate(name = factor(name, levels = unique(name)))


#graph
ggplot(clutch_graph_per_game, aes(x = n_per_game, y = (name), fill = situation)) +
  geom_col(col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = "top") +
  scale_x_continuous(expand = c(0.01,0)) +
  scale_fill_manual(values = c("#f1a226", "#298c8c")) +
  labs(x = "Career clutch goals per game", y = NULL, fill = NULL)
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/career_goals_per.jpg", width = 7, height = 9, dpi = 600)




########################################################
### Team level clutch offensive/defensive goal stats ###
########################################################



# convert boxcscores into long format to calculate team-level goals and scenarios
long_list <- list()
for (i in na.omit(unique(formatted$TeamId))) {
  
  temp_df <- formatted %>%
    filter(homeTeam == i | awayTeam == i) %>%
    mutate(team = i) %>%
    mutate(opponent = case_when(homeTeam == i ~ awayTeam,
                                awayTeam == i ~ homeTeam))
  
  long_list[[i + 1]] <- temp_df
  
}

formatted_with_clutch_long <- do.call(rbind, long_list)

formatted_with_clutch_long <- formatted_with_clutch_long %>%
  
  mutate(team_score = case_when(homeTeam == team ~ home_score,
                                awayTeam == team ~ away_score),
         
         opp_score = case_when(homeTeam == team ~ away_score,
                               awayTeam == team ~ home_score),
         
         last_team_score = case_when(homeTeam == team ~ last_home_score,
                                     awayTeam == team ~ last_away_score),
         
         last_opp_score = case_when(homeTeam == team ~ last_away_score,
                                    awayTeam == team ~ last_home_score)) %>%
  
  group_by(Game.Id) %>%
  
  mutate(ocs = case_when((Period == "3" & Time >= 1020 | Period == "OT1") & ((last_team_score - last_opp_score) %in% c(0,-1)) ~ TRUE,
                         (score == last(score)) & ((team_score - opp_score) %in% c(0,-1)) ~ TRUE,
                         TRUE ~ FALSE)) %>%
  
  mutate(dcs = case_when((Period == "3" & Time >= 1020 | Period == "OT1") & ((last_team_score - last_opp_score) %in% c(0,1)) ~ TRUE,
                         (score == last(score)) & ((team_score - opp_score) %in% c(0,1)) ~ TRUE,
                         TRUE ~ FALSE)) %>%
  
  mutate(ocg = case_when(TeamId == team & event_id %in% clutch$event_id ~ TRUE,
                         TRUE ~ FALSE)) %>%
  
  mutate(dcg = case_when(TeamId == opponent & event_id %in% clutch$event_id ~ TRUE,
                         TRUE ~ FALSE))


# summarize to team level
team_ocg_rate <- formatted_with_clutch_long %>%
  group_by(team) %>%
  summarise(ocg = sum(ocg),
            ocs = sum(ocs),
            rate = round(100*(ocg/ocs), 2),
            label = paste0("(", rate, "%)"),
            label_x = ocs + 20) %>%
  left_join(team_meta_merge, by = c("team" = "id")) %>%
  ungroup() %>%
  arrange(desc(rate)) %>%
  mutate(abbreviation = factor(abbreviation, levels = abbreviation))



ggplot(team_ocg_rate, aes(y = fct_rev(abbreviation), fill = abbreviation)) +
  geom_col(aes(x = ocs),
           col = "black",
           alpha = .33,
           show.legend = F) +
  geom_col(aes(x = ocg),
           col = "black",
           show.legend = F) +
  scale_fill_manual(values = team_colors) +
  geom_text(aes(x = ocs, label = label),
            hjust = -.2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_x_continuous(expand = c(0.01,0), limits = c(0,360)) +
  labs(x = "Total number of scenarios", y = NULL, title = "Offensive clutch conversion rate")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/team_offense.jpg", width = 6.5, height = 7.5, dpi = 600)


team_dcg_rate <- formatted_with_clutch_long %>%
  group_by(team) %>%
  summarise(dcg = sum(dcg),
            dcs = sum(dcs),
            stops = dcs-dcg,
            rate = round(100*(stops/dcs), 2),
            label = paste0("(", rate, "%)"),
            label_x = dcs + 100) %>%
  left_join(team_meta_merge, by = c("team" = "id")) %>%
  ungroup() %>%
  arrange(desc(rate)) %>%
  mutate(abbreviation = factor(abbreviation, levels = abbreviation))


ggplot(team_dcg_rate, aes(y = fct_rev(abbreviation), fill = abbreviation)) +
  geom_col(aes(x = -dcs),
           col = "black",
           alpha = .33,
           show.legend = F) +
  geom_col(aes(x = -stops),
           col = "black",
           show.legend = F) +
  scale_fill_manual(values = team_colors) +
  geom_text(aes(x = -dcs, label = label),
            hjust = 1.2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 1)) +
  
  scale_x_continuous(expand = c(0.01,0),
                     limits = c(-360, 0),
                     breaks = c(0,-100,-200,-300),
                     labels = c(0,100,200,300)) +
  
  scale_y_discrete(position = "right") +
  
  labs(x = "Total number of scenarios", y = NULL, title = "Defensive clutch success rate")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/team_defense.jpg", width = 6.5, height = 7.5, dpi = 600)






# Repeat on a season level
team_dcg_rate_season <- formatted_with_clutch_long %>%
  group_by(team, season) %>%
  summarise(dcg = sum(dcg),
            dcs = sum(dcs),
            stops = dcs-dcg,
            rate = round(100*(stops/dcs), 2),
            label = paste0("(", rate, "%)"),
            label_x = dcs + 100) %>%
  drop_na() %>%
  left_join(team_meta_merge, by = c("team" = "id")) %>%
  group_by(team) %>%
  mutate(summ = sum(rate)) %>%
  ungroup() %>%
  arrange(desc(summ)) %>%
  mutate(abbreviation = factor(abbreviation, levels = unique(abbreviation)))



ggplot(team_dcg_rate_season, aes(x = (season), y = (rate), col = abbreviation)) +
  geom_line(aes(group = abbreviation),
            show.legend = F) +
  geom_point(show.legend = F,
             shape = 21,
             size = 2) +
  facet_wrap(.~ abbreviation, ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  scale_color_manual(values = team_colors) +
  labs(x = "Season", y = "Defensive clutch conversion rate")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/team_defense_season.jpg", width = 7.5, height = 15, dpi = 600)

  


team_ocg_rate_season <- formatted_with_clutch_long %>%
  group_by(team, season) %>%
  summarise(ocg = sum(ocg),
            ocs = sum(ocs),
            rate = round(100*(ocg/ocs), 2),
            label = paste0("(", rate, "%)"),
            label_x = ocs + 20) %>%
  drop_na() %>%
  left_join(team_meta_merge, by = c("team" = "id")) %>%
  group_by(team) %>%
  mutate(summ = sum(rate)) %>%
  ungroup() %>%
  arrange(desc(summ)) %>%
  mutate(abbreviation = factor(abbreviation, levels = unique(abbreviation)))



ggplot(team_ocg_rate_season, aes(x = (season), y = (rate), col = abbreviation)) +
  geom_line(aes(group = abbreviation),
            show.legend = F) +
  geom_point(show.legend = F,
             shape = 21,
             size = 2) +
  facet_wrap(.~ abbreviation, ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  scale_color_manual(values = team_colors) +
  labs(x = "Season", y = "Offensive clutch conversion rate")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/team_offense_season.jpg", width = 7.5, height = 15, dpi = 600)






######################################################
### Cumulative plaer career clutch goals over time ###
######################################################

#first calculate every player's cumulative sum BY POSITION
#position swaps won't count towards their total
#label cumulative totals as character for the gif
cum_clutch <- clutch %>%
  ungroup() %>%
  arrange(season) %>%
  group_by(Scorer) %>%
  mutate(cum_clutch = cumsum(clutch),
         label = as.character(cum_clutch)) %>%
  arrange(desc(cum_clutch)) %>%
  ungroup() %>%
  left_join(team_meta_merge, by = c("TeamId" = "id")) %>%
  rename("team" = "abbreviation")


#iterate through all the seasons to build top 10 per season list
clutch_list <- list()
for (i in max(cum_clutch$season):min(cum_clutch$season)) {
  
  temp_clutch <- cum_clutch %>%
    filter(season <= i) %>%
    group_by(Scorer) %>%
    
    #this line is to make sure that only one player shows up in the top 10 list per season
    filter(cum_clutch == max(cum_clutch)) %>%
    ungroup() %>%
    mutate(rank = row_number(),
           season_label = i) %>%
    filter(rank <= 15)
  
  clutch_list[[i]] <- temp_clutch
}

clutch_over_time <- do.call(rbind, clutch_list)


#create a dummy df for one season earlier than minimum, to start everyone at 0
clutch_insert_df <- data.frame(season_label = 65,
                               cum_clutch = 0,
                               name = " ",
                               label = " ",
                               rank = 1:15,
                               
                               #need to create a fake team for color, shouldn't matter in the plot
                               team = "ATL")


#create a dummy df for two seasons later to hold the final results
clutch_insert_after1 <- filter(clutch_over_time, season_label == max(season_label)) %>% mutate(season_label = c(rep(max(seasons) + 1, 15)))
clutch_insert_after2 <- filter(clutch_over_time, season_label == max(season_label)) %>% mutate(season_label = c(rep(max(seasons) + 2, 15)))


#bind top 10 lists with dummy df
clutch_over_time <- bind_rows(clutch_over_time, clutch_insert_df, clutch_insert_after1, clutch_insert_after2) %>%
  arrange(season_label)



#plot
static_clutch <- ggplot(clutch_over_time, aes(rank, group = name, fill = team)) +
  geom_tile(aes(y = cum_clutch/2,
                height = cum_clutch,
                width = 0.9,
                fill = team), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = .5, hjust = 1, size = 10) +
  geom_text(aes(y= cum_clutch,label = label, hjust= -.25), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey80" ),
        panel.grid.minor.x = element_line( size=.1, color="grey80" ),
        plot.title=element_text(size=38, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=30, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=20, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2, 3, 2, 11.5, "cm"),
        panel.spacing = unit(12.5, "cm"),
        strip.text = element_text(size = 30)) +
  scale_fill_manual(values = team_colors)



#animate active skaters

anim_active = static_clutch + transition_states(season_label, transition_length = 4, state_length = 1) +
  #view_follow(fixed_x = TRUE)  +
  labs(title = paste('Career clutch goal leaders : {closest_state}\n\n', sep = ""))



animate(anim_active, 200, fps = 30,  duration = (length(seasons) +2)*1.5, width = 1500, height = 1000,
        renderer = gifski_renderer(paste("C://Users/Seth/Desktop/cumulative_cg.gif", sep = "")))





########################################################
### Looking at some of the most clutch playoff goals ###
########################################################



#Create small data frames of home/away team placeholders to join into the schedule
away_team_id <- select(team_meta, id, name, abbreviation)
colnames(away_team_id) <- c("id", "away.team", "away.abbreviation")

home_team_id <- select(team_meta, id, name, abbreviation)
colnames(home_team_id) <- c("id", "home.team", "home.abbreviation")






#Scrape the schedule and merge for all seasons in the league
schedule_list_playoffs <- list()
for (i in seasons) {
  schedule_url_playoffs <-GET("http://index.simulationhockey.com/api/v1/schedule", query = list(season = i, type = "playoffs"))
  schedule_url_playoffs <- fromJSON(rawToChar(schedule_url_playoffs$content))
  schedule_list_playoffs[[i]] <- schedule_url_playoffs
}

compiled_schedule_playoffs <- do.call(rbind, schedule_list_playoffs) %>%
  filter(type == "Playoffs")


po_teams_only <- compiled_schedule_playoffs %>% select(gameid, homeTeam, awayTeam)


#add the home/away team information to the compiled schedule
compiled_schedule_annotated_playoffs <- compiled_schedule_playoffs %>%
  left_join(away_team_id, by = c("awayTeam" = "id")) %>%
  left_join(home_team_id, by = c("homeTeam" = "id")) %>%
  
  #format the date to be an actual date and sort from oldest to newest
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  arrange(date)


#format the schedule to a tidy version
schedule_list_tidy_playoffs <- list()
for (teams in team_meta$name) {
  
  #filter out each team from the schedule to create an individual dataframe
  temp_schedule_playoffs <- compiled_schedule_annotated_playoffs %>%
    filter(away.team == teams | home.team == teams) %>%
    mutate(team = teams,
           opponent = case_when(away.team == teams ~ home.team,
                                home.team == teams ~ away.team))
  schedule_list_tidy_playoffs[[teams]] <- temp_schedule_playoffs
}
compiled_schedule_annotated_tidy_playoffs <- do.call(rbind, schedule_list_tidy_playoffs)



#format the OT column
formatted_schedule_playoffs <- compiled_schedule_annotated_tidy_playoffs %>%
  filter(type == "Playoffs") %>%
  mutate(win = case_when(team == home.team & (homeScore > awayScore) ~ TRUE,
                         team == away.team & (awayScore > homeScore) ~ TRUE,
                         TRUE ~ FALSE)) %>%
  group_by(season, team, opponent) %>%
  mutate(series_wins = cumsum(win),
         opp_wins = cumsum(!win),
         series_game = series_wins + opp_wins) %>%
  group_by(season, team) %>%
  mutate(n_series=cumsum(!duplicated(opponent))) %>%
  group_by(season, team, opponent) %>%
  mutate(clinching = case_when(lag(series_wins) == 3 ~ TRUE,
                               TRUE ~ FALSE),
         against_elim = case_when(lag(opp_wins) == 3 ~ TRUE,
                                  TRUE ~ FALSE))



# find all clutch goals from the playoffs
po_combined_boxscores <- combined_boxscores_all %>%
  filter(Game.Id %in% unique(formatted_schedule_playoffs$gameid))


# start to format for the clutch goal IDs
po_formatted <- po_combined_boxscores %>%
  left_join(po_teams_only, by = c("Game.Id" = "gameid")) %>%
  arrange(Game.Id, Period, Time) %>%
  group_by(Game.Id) %>%
  mutate(home_score = 1*(TeamId == homeTeam),
         away_score = 1*(TeamId == awayTeam),
         home_score = cumsum(home_score),
         away_score = cumsum(away_score)) %>%
  mutate(score = paste0(away_score, "-", home_score),
         score = case_when(score == "NA-NA" ~ "0-0",
                           TRUE ~ score)) %>%
  ungroup() %>%
  
  mutate(last_home_score = case_when(TeamId == homeTeam ~ home_score - 1,
                                     TeamId == awayTeam ~ home_score),
         last_away_score = case_when(TeamId == awayTeam ~ away_score - 1,
                                     TeamId == homeTeam ~ away_score)) %>%
  
  #create unique event id
  mutate(event_id = row_number())








##################################################
### Player level clutch goals and career stats ###
##################################################



# Find clutch goals
# Game tying or winning goals in the final 3 minutes or overtime
po_formatted_with_clutch <- po_formatted %>%
  filter(Period == "3" & Time >= 1020 | !(Period %in% c("1", "2", "3"))) %>%
  
  mutate(clutch = case_when(
    TeamId == homeTeam & ((last_home_score - last_away_score) %in% c(0,-1)) ~ TRUE,
    TeamId == awayTeam & ((last_away_score - last_home_score) %in% c(0,-1)) ~ TRUE,
    TRUE ~ FALSE
  ))


# filter for only clutch goals
po_clutch <- filter(po_formatted_with_clutch, clutch == TRUE)
po_clutch %>% group_by(Scorer) %>% summarise(n = n(), name = name[1]) %>% View()



#get a list of all teams with a clutch goal in the playoffs
po_game_id <- po_clutch %>%
  left_join(team_meta_merge, by = c("TeamId" = "id")) %>%
  group_by(Game.Id) %>%
  summarise(clutch_teams = paste(abbreviation, collapse = ","))


#clutch goals in clinching games
clutch_clinch <- formatted_schedule_playoffs %>%
  filter(clinching == TRUE) %>%
  filter(gameid %in% po_clutch$Game.Id) %>%
  left_join(po_game_id, by = c("gameid" = "Game.Id")) %>%
  select(season, homeScore, awayScore, team, opponent, clutch_teams, win, series_wins, opp_wins, series_game, n_series, gameid) %>%
  filter(win == TRUE)


#clutch goals against elimination
clutch_elim <- formatted_schedule_playoffs %>%
  filter(against_elim == TRUE) %>%
  filter(gameid %in% po_clutch$Game.Id) %>%
  left_join(po_game_id, by = c("gameid" = "Game.Id")) %>%
  select(season, homeScore, awayScore, team, opponent, clutch_teams, win, series_wins, opp_wins, series_game, n_series, gameid) %>%
  filter(win == TRUE)
  



#game line graph
#game ids: 8085, 10805, 13802, 14989, 15185
clutch_graph <- formatted %>%
  mutate(gametime = case_when(Period == "1" ~ Time,
                              Period == "2" ~ 1200 + Time,
                              Period == "3" ~ 2400 + Time,
                              Period == "OT1" ~ 3600 + Time))

graph <- clutch_graph %>%
  filter(Game.Id == 15185) %>%
  mutate(label = paste(name, score, sep = "\n"))
home_team <- team_meta$abbreviation[team_meta$id == unique(graph$homeTeam)]
away_team <- team_meta$abbreviation[team_meta$id == unique(graph$awayTeam)]
home_final <- last(graph$home_score)
away_final <- last(graph$away_score)
season <- unique(graph$season)


ggplot(graph, aes(x = gametime, y = home_score)) +
  geom_step(aes(y = home_score, col = factor(homeTeam)),
            size = 2,
            show.legend = F) +
  geom_step(aes(y = away_score, col = factor(awayTeam)),
            size = 2,
            show.legend = F) +
  scale_color_manual(values = team_color_abbr) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_label_repel(aes(label = label, col = factor(TeamId)),
                   alpha = .66,
                   show.legend = F) +
  scale_x_continuous(breaks = c(1200,2400,3600),
                     labels = c("1", "2", "3")) +
  labs(title = paste0(away_team, " (", away_final, ") vs. ", home_team, " (", home_final, ") : Season ", season),
       x = "Period",
       y = "Score")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/game5.png", width = 10, height = 5, dpi = 600)





################################################################################################
########################### GOALIE ADDITION TO THE PEICE #######################################
################################################################################################


# This section is kind of a mess, just hope you never have to revisit this code i guess

### Get a list of playerIDs for the goalie map
#get a list of player stats
# to merge names to IDs and for data validation
goalie_list <- list()
for (i in seasons) {
  goalie_stats <- GET("http://index.simulationhockey.com/api/v1/goalies/stats", query = list(season = i))
  goalie_stats <- fromJSON(rawToChar(goalie_stats$content))
  goalie_stats <- do.call(data.frame, goalie_stats)
  goalie_list[[i]] <- goalie_stats
}
combined_goalie_stats <- do.call(rbind, goalie_list)

#get just their names and IDs for merging
goalie_id_map <- combined_goalie_stats %>%
  select(name, id) %>%
  group_by(id) %>%
  summarise(name = last(name))




### Load the goalie boxscores 
boxscore_directory <- "C://Users/Seth/Desktop/clutch media/"
subfolders <- list.files(boxscore_directory)
subfolders <- subfolders[!subfolders == "Graphs"]


g_boxscore_list <- list()
for (i in subfolders) {
  temp_directory <- paste0(boxscore_directory, i)
  temp_boxscore <- read.csv(paste0(temp_directory, "/boxscore_goalie_summary.csv"),  
                            sep = ";")
  temp_boxscore$season <- i
  g_boxscore_list[[i]] <- temp_boxscore
}

combined_boxscores_g <- do.call(rbind, g_boxscore_list)


#filter for regular season only by checking for game IDs in the merged schedule fule
g_plyoff_boxscores <- combined_boxscores_g %>%
  filter(Game.Id %in% po_formatted$Game.Id) %>%
  filter(SV. != ".00nan") %>%
  mutate(sv_pct = SV/SA) %>%
  
  #merge with meta data
  left_join(team_meta_merge, by = c("TeamId" = "id")) %>%
  left_join(goalie_id_map, by = c("PlayerId" = "id")) %>%
  group_by(abbreviation, Game.Id) %>%
  mutate(n = n())



#add total wins to schedule
formatted_schedule_playoffs_graph <- formatted_schedule_playoffs %>%
  left_join(select(team_meta, name, abbreviation, conference, division), by = c("team" = "name")) %>%
  group_by(abbreviation, season) %>%
  mutate(total_wins = cumsum(win)) %>%
  filter(total_wins > 0) %>%
  select(season, abbreviation, opponent, total_wins, n_series, series_wins, conference, division) %>%
  distinct() %>%
  mutate(row = paste0(conference, division)) %>%
  ungroup()%>%
  arrange(row) %>%
  mutate(abbreviation = factor(abbreviation, levels = unique(abbreviation)))



ggplot(formatted_schedule_playoffs_graph, aes(x = season, y = total_wins, fill = factor(n_series), alpha = factor(series_wins))) +
  geom_tile(col = "black",
            show.legend = F) +
  facet_wrap(.~ abbreviation) +
  scale_fill_manual(values = c("springgreen4","dodgerblue2", "purple3", "red3")) +
  scale_alpha_manual(values = c(0,.25,.5,.75,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0.025,0))



#playoff matchup matrix
matchup_matrix <- formatted_schedule_playoffs %>%
  group_by(team, opponent, season) %>%
  summarise(series_win = case_when(sum(win) == 4 ~ TRUE,
                                   TRUE ~ FALSE)) %>%
  group_by(team, opponent) %>%
  mutate(opp_number = length(unique(season))) %>%
  mutate(opp_wins = sum(series_win),
         opp_losses = opp_number-opp_wins,
         opp_perc = opp_wins/opp_number) %>%
  group_by(team) %>%
  mutate(number = n(),
            wins = sum(series_win),
            losses = number-wins,
            perc = wins/number) 


ggplot(matchup_matrix, aes(y = fct_rev(team), x =(opponent))) +
  geom_point(aes(fill = factor(opp_wins), size = opp_number),
             shape = 21,
             col = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 1)) +
  scale_x_discrete(position = "top") +
  scale_fill_viridis_d(option = "A") +
  scale_size_continuous(range = c(2,10), breaks = c(2:10), labels = c("2","3","4","5","6","7","8","9","10")) +
  labs(x = "Opponent", y = "Team", size= "Total matchups", fill = "Series wins")




# This section begins the long process of assigning a G win to a specific game
goalies_formatted <- g_plyoff_boxscores %>%
  filter(TOI > 0) %>%
  group_by(Game.Id, TeamId) %>%
  mutate(n = n()) %>%
  mutate(GA = as.numeric(GA),
         time = floor(as.numeric(TOI/1000)))


po_boxscores_for_g <- po_formatted %>%
  mutate(total_time = case_when(Period == "1" ~ Time,
                                Period == "2" ~ 1200 + Time,
                                Period == "3" ~ 2400 + Time,
                                Period == "OT1" ~ 3600 + Time))

boxscores_list <- list()
for (i in unique(c(po_boxscores_for_g$homeTeam, po_boxscores_for_g$awayTeam))) {
  temp_df <- po_boxscores_for_g %>%
    filter(homeTeam == i | awayTeam == i) %>%
    mutate(team = i,
           opponent = case_when(team == homeTeam ~ awayTeam,
                                team == awayTeam ~ homeTeam)) %>%
    mutate(team_score = case_when(team == homeTeam ~ home_score,
                                  team == awayTeam ~ away_score),
           opp_score = case_when(team == homeTeam ~ away_score,
                                 team == awayTeam ~ home_score))
  
  boxscores_list[[i+1]] <- temp_df
  
}
po_boxscores_long <- do.call(rbind, boxscores_list)



boxscores_long_merge <- po_boxscores_long %>%
  select(team, Game.Id, total_time, team_score, opp_score) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(adj_time = case_when(total_time %in% goalies_formatted$time[goalies_formatted$Game.Id == Game.Id & goalies_formatted$TeamId == team] ~ total_time,
                              TRUE ~ total_time - 1))


box_score_results <- po_boxscores_long %>%
  group_by(Game.Id, team) %>%
  summarise(final_score = last(team_score),
            final_opp_score = last(opp_score),
            win = final_score > final_opp_score)











g_win_game_merge <- goalies_formatted %>%
  left_join(boxscores_long_merge, by = c("Game.Id", "GA" = "opp_score", "time" = "adj_time", "TeamId" = "team")) %>%
  mutate(role = case_when(n == 1 ~ "Starter",
                          n == 2 & !is.na(team_score) ~ "Starter",
                          TRUE ~ "Backup")) %>%
  left_join(box_score_results, by = c("Game.Id", "TeamId" = "team")) 

g_win_game_merge$role[g_win_game_merge$Game.Id == 758 & g_win_game_merge$name == "Cillian Kavanagh"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 758 & g_win_game_merge$name == "Cillian Kavanagh"] <- 0

g_win_game_merge$role[g_win_game_merge$Game.Id == 2550 & g_win_game_merge$name == "Rebecca Montagne"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 2550 & g_win_game_merge$name == "Rebecca Montagne"] <- 4

g_win_game_merge$role[g_win_game_merge$Game.Id == 2552 & g_win_game_merge$name == "Toms Zile"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 2552 & g_win_game_merge$name == "Toms Zile"] <- 4

g_win_game_merge$role[g_win_game_merge$Game.Id == 6772 & g_win_game_merge$name == "B Jobin"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 6772 & g_win_game_merge$name == "B Jobin"] <- 6

g_win_game_merge$role[g_win_game_merge$Game.Id == 8442 & g_win_game_merge$name == "Rusty Remao"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 8442 & g_win_game_merge$name == "Rusty Remao"] <- 4

g_win_game_merge$role[g_win_game_merge$Game.Id == 9357 & g_win_game_merge$name == "Mat Smith"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 9357 & g_win_game_merge$name == "Mat Smith"] <- 4

g_win_game_merge$role[g_win_game_merge$Game.Id == 11962 & g_win_game_merge$name == "Tummy Hurts"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 11962 & g_win_game_merge$name == "Tummy Hurts"] <- 6

g_win_game_merge$role[g_win_game_merge$Game.Id == 14678 & g_win_game_merge$name == "BASE PACK"] <- "Starter"
g_win_game_merge$team_score[g_win_game_merge$Game.Id == 14678 & g_win_game_merge$name == "BASE PACk"] <- 4




one_g_games <- g_win_game_merge %>% filter(n == 1) %>% mutate(goalie_game_win = win)
two_g_games <- g_win_game_merge %>% filter(n == 2)

two_g_games_win <- two_g_games %>%
  group_by(Game.Id, TeamId) %>%
  mutate(goalie_game_win = case_when(
    
    #starter is winning but still earns the win
    win == TRUE & role == "Starter" & team_score > GA & final_score > GA ~ TRUE,
    #starter is winning but the team loses
    win == FALSE & role == "Starter" & team_score >= GA ~ NA,
    #starter is losing/tied and team wins
    win == TRUE & role == "Starer" & team_score <= GA ~ NA,
    #starter is losing and team loses 
    win == FALSE & role == "Starter" & team_score <= GA & GA >= final_score ~ FALSE,
    #starter is losing but the backup chokes more
    win == FALSE & role == "Starter" & final_score >= GA ~ NA)) %>%
  
  mutate(goalie_game_win = case_when(
    !(is.na(goalie_game_win)) ~ goalie_game_win,
    win == TRUE & role == "Backup" & is.na(goalie_game_win[role == "Starter"]) ~ TRUE,
    win == TRUE & role == "Backup" & !(is.na(goalie_game_win[role == "Starter"])) ~ NA,
    win == FALSE & role == "Backup" & is.na(goalie_game_win[role == "Starter"]) ~ FALSE,
    win == FALSE & role == "Backup" & !(is.na(goalie_game_win[role == "Starter"])) ~ NA))

g_win_merge <- rbind(one_g_games, two_g_games_win) 

g_win_select <- g_win_merge %>%
  ungroup() %>%
  select(Game.Id, SA:SV, sv_pct, name, abbreviation, goalie_game_win)



####################################################################


# Merge goalie wins with formatted playoffs

formatted_schedule_playoffs_goalies <- formatted_schedule_playoffs %>%
  mutate(team_abbr = case_when(team == home.team ~ home.abbreviation,
                               team == away.team ~ away.abbreviation),
         opp_abbr = case_when(team == home.team ~ away.abbreviation,
                              team == away.team ~ home.abbreviation)) %>%
  left_join(g_win_select, by = c("gameid" = "Game.Id", "team_abbr" = "abbreviation"))




# Career playoff stats
career_g_po <- read.csv("C://Users/Seth/Desktop/clutch media/Graphs/player_goalie_career_stats_po.csv", sep = ";")
career_g_po_stats <- career_g_po %>%
  group_by(PlayerId) %>%
  summarise(wins = sum(W),
            loss = sum(L) + sum(T.OL),
            save_pct = (sum(SA) - sum(GA))/sum(SA),
            SA = sum(SA),
            SA_game = sum(SA)/sum(GP),
            gp = sum(GP),
            w_pct = wins/(wins + loss)) %>%
  left_join(goalie_id_map, by = c("PlayerId" = "id"))



career_g_po_graph <- career_g_po_stats %>%
  filter(gp > 10) 
  
ggplot(career_g_po_graph, aes(x = save_pct, y = w_pct)) +
  geom_point(aes(size = gp, fill = SA_game),
             shape = 21,
             col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_vline(xintercept = .9, linetype = "dashed") +
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "red3", midpoint = 32.5) +
  scale_size_continuous(range = c(1,10)) +
  geom_text_repel(aes(label = name)) +
  labs(x = "Career PO save pct", y = "Career PO win pct", fill = "Shots against/game", size = "GP")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/G_career_scatter.jpg", width = 11, height = 9.5, dpi = 600)



# career clinching playoff stats
g_career_clinch_season <- formatted_schedule_playoffs_goalies %>%
  filter(clinching == TRUE) %>% 
  group_by(name, season) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win + loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)


g_career_clinch <- formatted_schedule_playoffs_goalies %>%
  filter(clinching == TRUE) %>% 
  group_by(name) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win+loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)

g_career_clinch %>%
  filter(win + loss > 4) %>%
  
  ggplot(aes(x = save_pct, y = win_pct)) +
  geom_point(aes(size = GP, fill = SA_game),
             shape = 21,
             col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_vline(xintercept = .9, linetype = "dashed") +
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "red3", midpoint = 32.5) +
  scale_size_continuous(range = c(1,10)) +
  geom_text_repel(aes(label = name)) +
  labs(x = "Career PO save pct", y = "Career PO win pct", fill = "Shots against/game", size = "GP", title = "Career stats in clinching games")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/G_clinch_scatter.jpg", width = 11, height = 9.5, dpi = 600)



# career elim playoff stats
g_career_elim_season <- formatted_schedule_playoffs_goalies %>%
  filter(against_elim == TRUE) %>% 
  group_by(name, season) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win + loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)


g_career_elim <- formatted_schedule_playoffs_goalies %>%
  filter(against_elim == TRUE) %>% 
  group_by(name) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win+loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)

g_career_elim %>%
  filter(win + loss > 4) %>%
  
  ggplot(aes(x = save_pct, y = win_pct)) +
  geom_point(aes(size = GP, fill = SA_game),
             shape = 21,
             col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_vline(xintercept = .9, linetype = "dashed") +
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "red3", midpoint = 32.5) +
  scale_size_continuous(range = c(1,10)) +
  geom_text_repel(aes(label = name)) +
  labs(x = "Career PO save pct", y = "Career PO win pct", fill = "Shots against/game", size = "GP", title = "Career stats in elimination games")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/G_elim_scatter.jpg", width = 11, height = 9.5, dpi = 600)





# career g7 stats
g_career_g7_season <- formatted_schedule_playoffs_goalies %>%
  filter(series_game == 7) %>% 
  group_by(name, season) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win + loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)


g_career_g7 <- formatted_schedule_playoffs_goalies %>%
  filter(series_game == 7) %>% 
  group_by(name) %>%
  summarise(win = sum(goalie_game_win, na.rm = T),
            loss = sum(!goalie_game_win, na.rm = T),
            win_pct = win/(win+loss),
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP)

g_career_g7 %>%
  filter(win + loss > 3) %>%
  
  ggplot(aes(x = save_pct, y = win_pct)) +
  geom_point(aes(size = GP, fill = SA_game),
             shape = 21,
             col = "black") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_vline(xintercept = .9, linetype = "dashed") +
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "red3", midpoint = 32.5) +
  scale_size_continuous(range = c(1,10)) +
  geom_text_repel(aes(label = name)) +
  labs(x = "Career PO save pct", y = "Career PO win pct", fill = "Shots against/game", size = "GP", title = "Career stats in game 7")
ggsave("C://Users/Seth/Desktop/clutch media/Graphs/G_g7_scatter.jpg", width = 11, height = 9.5, dpi = 600)



#################################### best performances of all time ###############################

# best clinching performances 
g_clinch_perf <- formatted_schedule_playoffs_goalies %>%
  filter(clinching == TRUE) %>% 
  filter(!is.na(goalie_game_win)) %>%
  arrange(desc(sv_pct), desc(SV))

g_elim_perf <- formatted_schedule_playoffs_goalies %>%
  filter(against_elim == TRUE) %>% 
  filter(!is.na(goalie_game_win)) %>%
  arrange(desc(sv_pct), desc(SV))


g_g7_perf <- formatted_schedule_playoffs_goalies %>%
  filter(series_game == 7) %>% 
  filter(!is.na(goalie_game_win)) %>%
  arrange(desc(sv_pct), desc(SV))




# best series perf


#scrape team standings
standings_list <- list()
for (i in seasons) {
  standings <-GET("http://index.simulationhockey.com/api/v1/standings", query = list(season = i))
  standings <- fromJSON(rawToChar(standings$content))
  standings <- do.call(data.frame, standings)
  standings$season <- i
  standings_list[[i]] <- standings
}

compiled_standings <- do.call(rbind, standings_list)

best_series <- formatted_schedule_playoffs_goalies %>%
  ungroup() %>%
  mutate(series_id = paste0(season, team, opponent)) %>%
  group_by(series_id) %>%
  filter(max(series_wins) == 4) %>%
  group_by(name, series_id) %>%
  summarise(n_series = n_series[1],
            season = season[1],
            team = team[1],
            opp = opponent[1],
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP) %>%
  filter(GP >= 4) %>%
  ungroup() %>%
  arrange(desc(save_pct), desc(SA)) 

best_series$team_points <- NA
best_series$opp_points <- NA

for (i in 1:nrow(best_series)) {
  best_series$team_points[i] <- compiled_standings$points[compiled_standings$name == best_series$team[i] & compiled_standings$season == best_series$season[i]]
  best_series$opp_points[i] <- compiled_standings$points[compiled_standings$name == best_series$opp[i] & compiled_standings$season == best_series$season[i]]
}

best_series$point_diff = best_series$team_points - best_series$opp_points


ggplot(best_series, aes(x = SA, y = GA)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F)

model <- lm(GA ~ SA, data = best_series)

best_series$pred <- predict(model)
best_series$diff <- best_series$pred - best_series$GA



best_playoff_run <- formatted_schedule_playoffs_goalies %>%
  group_by(name, season) %>%
  summarise(n_series = max(n_series),
            season = season[1],
            team = team[1],
            GA = sum(GA),
            SA = sum(SA),
            SV = sum(SV),
            save_pct = SV/SA,
            GP = n(),
            SA_game = SA/GP) %>%
  arrange(desc(save_pct), desc(SA)) 

  