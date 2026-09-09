library(tidyverse)
library(httr)
library(jsonlite)
library(ggh4x)


source("scraper_functions.R")



###################################
### Data loading and formatting ###
###################################
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


last <- function(x) { return( x[length(x)] ) }


# Load the files
### Either with an appended scrape, or directly from .csv if it is up-to-date
team_meta <- index_meta(seasons = 89)
schedule <- index_schedule(89, append = T)
combined_player_stats <- index_player_stats(89, append = T)
combined_goalie_stats <- index_goalie_stats(89, append = T)
combined_boxscores_all <- file_scoring_summary(seasons = 89, append = T)



# Set up meta file and team colors
team_meta_merge <- team_meta %>% 
  select(id, abbreviation)
team_colors <- team_meta$colors_primary
names(team_colors) <- team_meta$abbreviation


#Isolate games that ended 0-0 in regulation/OT
shootout_shutout <- schedule %>%
  filter(shootout == 1 & homeScore + awayScore == 1) %>%
  select(gameid, homeTeam, awayTeam, season) %>%
  mutate(season = as.character(season)) %>%
  rename("Game.Id" = "gameid")

teams_only <- schedule %>% select(gameid, homeTeam, awayTeam)




#get just player names and IDs for merging
player_id_map <- combined_player_stats %>%
  select(name, id) %>%
  group_by(id) %>%
  summarise(name = last(name))


#get position maps
player_pos_map <- combined_player_stats %>%
  select(season, id, position) %>%
  mutate(position = case_when(position %in% c("LD", "RD") ~ "Defense",
                               TRUE ~ "Forward"))



#get just their names and IDs for merging
goalie_id_map <- combined_goalie_stats %>%
  select(name, id) %>%
  group_by(id) %>%
  summarise(name = last(name))




#filter boxscores for regular season only by checking for game IDs in the merged schedule fule
combined_boxscores <- combined_boxscores_all %>%
  filter(Game.Id %in% schedule$gameid) %>%
  mutate(season = as.numeric(as.character(season)))


formatted_boxscores <- combined_boxscores %>%
  mutate(goal_id = row_number()) %>%
  pivot_longer(cols = c(Scorer, Assist.1, Assist.2),
               names_to = "role",
               values_to = "player") %>%
  
  #filter for only goals with 3 teammates
  filter(!is.na(player)) %>%
  group_by(goal_id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  
  #merge with player and meta meta info and format
  left_join(player_id_map, by = c("player" = "id")) %>%
  left_join(player_pos_map, by = c("player" = "id", "season")) %>%
  group_by(goal_id) %>%
  arrange(name) %>%
  mutate(trio = paste(name, collapse = " | ")) %>%
  left_join(select(team_meta, id, abbreviation), by = c("TeamId" = "id"))



# Check if there are any trios that exist on multiple teams
num_team <- formatted_boxscores %>% 
  group_by(trio, abbreviation) %>% 
  summarise(team_goals = n_distinct(goal_id)) %>%
  group_by(trio) %>%
  mutate(n_teams = n_distinct(abbreviation),
         total = sum(team_goals)) 

top20 <- num_team %>%
  group_by(trio) %>%
  summarise(n = total[1]) %>%
  arrange(desc(n)) %>%
  filter(row_number() <= 20)




# top 20 plot
top20_plot <- formatted_boxscores %>%
  filter(trio %in% top20$trio) %>%
  group_by(trio, player) %>%
  summarise(goals = sum(role == "Scorer"),
            name = Mode(name),
            team = paste(unique(abbreviation), collapse = "/")) %>%
  group_by(trio) %>%
  arrange(desc(goals)) %>%
  mutate(trio_label = paste(unique(name), collapse = " | ")) %>%
  mutate(trio_label = paste0("(", team, ") ", trio_label)) %>%
  mutate(trio_n = sum(goals)) %>%
  ungroup() %>%
  arrange(desc(trio_n)) %>%
  mutate(trio_label = factor(trio_label, levels = unique(trio_label)))


#labels 
labels <- top20_plot %>%
  group_by(trio_label) %>%
  summarise(n = sum(goals)) %>%
  mutate(x_pos = n + 10,
         label = paste0("(", n, ")")) 


ggplot(top20_plot, aes(x = goals, y= fct_rev(trio_label))) +
  geom_col(aes(fill = goals),
           position = "stack", 
           show.legend = F,
           col = "black") +
  geom_text(aes(label = goals), position = position_stack(vjust = .5)) +
  geom_text(data = labels, aes(x = x_pos, label = label)) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_gradient2(high = "#2c7fb8",mid = "#7fcdbb", low = "#edf8b1", midpoint = 30) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/trio_top20.jpg", width = 11.5, height = 10, dpi = 300)





### Repeat for trios with at least 1 Dman
num_team <- formatted_boxscores %>% 
  group_by(trio, season, goal_id) %>%
  mutate(d_inc = ("Defense" %in% position)) %>%
  filter(d_inc == TRUE) %>%
  group_by(trio, abbreviation) %>% 
  summarise(team_goals = n_distinct(goal_id)) %>%
  group_by(trio) %>%
  mutate(n_teams = n_distinct(abbreviation),
         total = sum(team_goals)) 

top20 <- num_team %>%
  group_by(trio) %>%
  summarise(n = total[1]) %>%
  arrange(desc(n)) %>%
  filter(row_number() <= 20)


# top 20 plot
top20_plot <- formatted_boxscores %>%
  filter(trio %in% top20$trio) %>%
  group_by(trio, season, goal_id) %>%
  mutate(d_inc = ("Defense" %in% position)) %>%
  filter(d_inc == TRUE) %>%
  group_by(trio, player) %>%
  summarise(goals = sum(role == "Scorer"),
            name = Mode(name),
            team = paste(unique(abbreviation), collapse = "/")) %>%
  group_by(trio) %>%
  arrange(desc(goals)) %>%
  mutate(trio_label = paste(unique(name), collapse = " | ")) %>%
  mutate(trio_label = paste0("(", team, ") ", trio_label)) %>%
  mutate(trio_n = sum(goals)) %>%
  ungroup() %>%
  arrange(desc(trio_n)) %>%
  mutate(trio_label = factor(trio_label, levels = unique(trio_label)))


#labels 
labels <- top20_plot %>%
  group_by(trio_label) %>%
  summarise(n = sum(goals)) %>%
  mutate(x_pos = n + 4,
         label = paste0("(", n, ")")) 


ggplot(top20_plot, aes(x = goals, y= fct_rev(trio_label))) +
  geom_col(aes(fill = goals),
           position = "stack", 
           show.legend = F,
           col = "black") +
  geom_text(aes(label = goals), position = position_stack(vjust = .5)) +
  geom_text(data = labels, aes(x = x_pos, label = label)) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_gradient2(high = "#2c7fb8",mid = "#7fcdbb", low = "#edf8b1", midpoint = 20) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/trio_d_inc_top20.jpg", width = 11.5, height = 10, dpi = 300)

  

### Multi team trios
multi_teams <- formatted_boxscores %>%
  group_by(trio, abbreviation) %>% 
  summarise(n = n()/3) %>% 
  group_by(trio) %>% 
  mutate(n2 = n()) %>% 
  filter(n2 > 1) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(trio = factor(trio, levels = unique(trio)))

ggplot(multi_teams, aes(x = n, y = fct_rev(trio), fill = abbreviation)) +
  geom_col(
           position = "stack", 
           show.legend = F,
           col = "black") +
  geom_text(aes(label = n), position = position_stack(vjust = .5), col = "white") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_manual(values = team_colors) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/multiteams.jpg", width = 11.5, height = 10, dpi = 300)

  
## team top trios
# top 20 plot
franchise_leader <- formatted_boxscores %>%
  group_by(trio, player, abbreviation) %>%
  summarise(goals = sum(role == "Scorer"),
            name = Mode(name),
            team = abbreviation[1]) %>%
  group_by(trio) %>%
  arrange(desc(goals)) %>%
  mutate(trio_label = paste(unique(name), collapse = " | ")) %>%
  mutate(trio_n = sum(goals)) %>%
  ungroup() %>%
  arrange(desc(trio_n)) %>%
  group_by(team) %>%
  filter(row_number() <= 9) %>%
  mutate(trio_label = factor(trio_label, levels = unique(trio_label)))

colors <- team_meta %>% arrange(abbreviation)
strip <- strip_themed(background_x = elem_list_rect(fill = alpha(colors$colors$primary, .59)))

ggplot(franchise_leader, aes(x = goals, y= fct_rev(trio_label))) +
  geom_col(aes(fill = goals),
           position = "stack", 
           show.legend = F,
           col = "black") +
  facet_wrap2(.~ team, scale = "free_y", ncol = 2, strip = strip) +
  geom_text(aes(label = goals), position = position_stack(vjust = .5)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +
        scale_fill_gradient2(high = "#2c7fb8",mid = "#7fcdbb", low = "#edf8b1", midpoint = 30) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/team_leaders.jpg", width = 23, height = 13, dpi = 300)





### single season high


## team top trios
# top 20 plot
season_leader <- formatted_boxscores %>%
  group_by(trio, player, abbreviation, season) %>%
  summarise(goals = sum(role == "Scorer"),
            name = Mode(name),
            team = abbreviation[1],
            season = season[1]) %>%
  group_by(trio, season) %>%
  arrange(desc(goals)) %>%
  mutate(trio_label = paste(unique(name), collapse = " | "),
         trio_label = paste0(trio_label, " (", team, ", ", "S", season,")"),
         trio_n = sum(goals)) %>%
  ungroup() %>%
  arrange(desc(trio_n)) %>%
  filter(row_number() <= 60) %>%
  mutate(trio_label = factor(trio_label, levels = unique(trio_label)))


ggplot(season_leader, aes(x = goals, y= fct_rev(trio_label))) +
  geom_col(aes(fill = goals),
           position = "stack", 
           show.legend = F,
           col = "black") +
  geom_text(aes(label = goals), position = position_stack(vjust = .5)) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_gradient2(high = "#2c7fb8",mid = "#7fcdbb", low = "#edf8b1", midpoint = 10) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/season_leaders.jpg", width = 11.5, height = 10, dpi = 300)





## team top trios that include at least 1 dman
# top 20 plot
season_leader <- formatted_boxscores %>%
  group_by(trio, season) %>%
  mutate(d_inc = ("Defense" %in% position)) %>%
  filter(d_inc == TRUE) %>%
  group_by(trio, player, abbreviation, season) %>%
  summarise(goals = sum(role == "Scorer"),
            name = Mode(name),
            team = abbreviation[1],
            season = season[1]) %>%
  group_by(trio, season) %>%
  arrange(desc(goals)) %>%
  mutate(trio_label = paste(unique(name), collapse = " | "),
         trio_label = paste0(trio_label, " (", team, ", ", "S", season,")"),
         trio_n = sum(goals)) %>%
  ungroup() %>%
  arrange(desc(trio_n)) %>%
  filter(row_number() <= 60) %>%
  mutate(trio_label = factor(trio_label, levels = unique(trio_label)))


ggplot(season_leader, aes(x = goals, y= fct_rev(trio_label))) +
  geom_col(aes(fill = goals),
           position = "stack", 
           show.legend = F,
           col = "black") +
  geom_text(aes(label = goals), position = position_stack(vjust = .5)) +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_gradient2(high = "#2c7fb8",mid = "#7fcdbb", low = "#edf8b1", midpoint = 6) +
  labs(x = "Goals", y = NULL)
ggsave("C://Users/Seth/Desktop/season_leaders_d_inc.jpg", width = 11.5, height = 10, dpi = 300)





#### 

# goals against each team

opp_vectors <- schedule %>%
  select(gameid, homeTeam, awayTeam) %>%
  mutate(team = homeTeam, 
         opp = awayTeam) %>%
  pivot_longer(c(team, opp),
               names_to = "team",
               values_to = "id") %>%
  group_by(gameid) %>%
  mutate(opp = case_when(id == homeTeam ~ awayTeam,
                         id == awayTeam ~ homeTeam)) %>%
  ungroup()
  

team_opp_formatted <- formatted_boxscores %>%
  select(-trio) %>%
  left_join(select(opp_vectors, id, opp, gameid), by = c("TeamId" = "id", "Game.Id" = "gameid")) %>%
  left_join(select(team_meta, id, abbreviation), by = c("opp" = "id")) %>%
  select(-opp) %>%
  rename("opp" = "abbreviation.y",
         "team" = "abbreviation.x")



team_goals <- team_opp_formatted %>%
  filter(role=="Scorer") %>%
  group_by(player, opp, team) %>%
  summarise(n = n(),
            name = Mode(name)) %>%
  group_by(opp, name) %>%
  mutate(opp_total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(opp_total)) %>%
  mutate(label = paste0(name, opp)) %>%
  group_by(opp) 

#create totals separately
team_opp_total_rank <- team_goals %>%
  group_by(opp, name) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  group_by(opp) %>%
  mutate(rank = rank(-total, ties.method = "min"))

  
#merge back
team_goals <- team_goals %>% 
  left_join(team_opp_total_rank) %>%
  filter(rank <= 3) %>%
  arrange(desc(opp_total)) %>%
  mutate(label = factor(label, levels = unique(label)))



ggplot(team_goals, aes(x = n, y = fct_rev(label))) +
  geom_col(aes(fill = team),
           col = "black",
           show.legend = F) +
  facet_wrap(.~ opp, scales = "free_y", ncol = 2) +
  scale_y_discrete(breaks = team_goals$label, labels = team_goals$name) +
  scale_fill_manual(values = team_colors) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(y = NULL, x = "Goals")
ggsave("C://Users/Seth/Desktop/opp_goals.jpg", width = 12, height = 14, dpi = 300)



# palyer poitns against
team_points <- team_opp_formatted %>%
  group_by(player, role, opp) %>%
  summarise(n = n(),
            name = Mode(name))

#split to rank and then merge back  
team_points_rank <- team_points %>%
  group_by(name, opp) %>%
  summarise(points = sum(n)) %>%
  group_by(opp) %>%
  arrange(desc(points)) %>%
  mutate(rank = rank(-points, ties.method = "min"))


#merge back
team_points <- team_points %>%
  left_join(team_points_rank) %>%
  filter(rank <= 3) %>%
  mutate(role = factor(role, levels = c("Scorer", "Assist.1", "Assist.2"),
                       labels = c("G", "A1", "A2"))) %>%
  group_by(opp) %>%
  arrange(rank) %>%
  mutate(label = paste0(name, opp)) %>%
  mutate(label = factor(label, levels = unique(label)))

ggplot(team_points, aes(x = n, y = fct_rev(label))) +
  geom_col(aes(fill = fct_rev(role)),
           col = "black") +
  facet_wrap(.~ opp, scales = "free_y", ncol = 2) +
  scale_y_discrete(breaks = team_points$label, labels = team_points$name) +
  scale_fill_manual(values = c("#2c7fb8","#7fcdbb","#edf8b1")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = NULL, x = "Points", fill = NULL)
ggsave("C://Users/Seth/Desktop/opp_points.jpg", width = 12, height = 14, dpi = 300)





### skater differential points against

# first, load all boxscore summary files, to summarize games player per opponent
box_summary_all <- read.csv("C://Users/Seth/Desktop/boxscore_skater_summary_all.csv")
boxscore_summary_all <- box_summary_all %>%
  filter(SeasonID < 83) %>%
  filter(gameID %in% schedule$gameid) %>%
  select(gameID, playerID, teamId) %>%
  arrange(gameID, teamId, playerID) %>%
  group_by(gameID) %>%
  mutate(oppId = rev(teamId))

player_gp_opp <- boxscore_summary_all %>%
  left_join(select(team_meta, id, abbreviation), by = c("oppId" = "id")) %>%
  group_by(playerID, abbreviation) %>%
  summarise(gp = n())


# format the df
team_points_w_wo <- team_opp_formatted %>%
  group_by(player, opp) %>%
  summarise(points = n(),
            name = Mode(name)) %>%
  group_by(player) %>%
  left_join(player_gp_opp, by = c("player" = "playerID", "opp" = "abbreviation")) %>%
  mutate(ppg = points/gp) %>%
  group_by(player) %>%
  mutate(lfc = log(ppg/mean(ppg))) %>%
  
  #filter by GP per opponent
  filter(min(gp) >= 14)



heatmap <- team_points_w_wo %>%
  ungroup() %>%
  arrange(desc(lfc)) %>%
  mutate(rank = row_number()) %>%
  group_by(player) %>%
  filter(min(rank) <= 26) %>%
  ungroup() %>%
  select(name, opp, ppg, lfc) %>%
  mutate(name = factor(name, levels = unique(name)),
         opp = factor(opp, levels = unique(opp))) 

ggplot(heatmap, aes(x = opp, y = fct_rev(name), fill = lfc)) +
  geom_tile(col = "black",
            show.legend = F) +
  geom_text(aes(label = round(ppg, 2))) +
  scale_fill_gradient2(low = "steelblue4", mid = "grey90", high = "red3", midpoint = .2) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave("C://Users/Seth/Desktop/points_heatmap.png", height = 7, width = 10, dpi = 300)



### reverse order heatmap
heatmap_rev <- team_points_w_wo %>%
  ungroup() %>%
  arrange(lfc) %>%
  mutate(rank = row_number()) %>%
  group_by(player) %>%
  filter(min(rank) <= 27) %>%
  ungroup() %>%
  select(name, opp, ppg, lfc) %>%
  mutate(name = factor(name, levels = unique(name)),
         opp = factor(opp, levels = unique(opp))) 


ggplot(heatmap_rev, aes(x = opp, y = fct_rev(name), fill = lfc)) +
  geom_tile(col = "black",
            show.legend = F) +
  geom_text(aes(label = round(ppg, 2))) +
  scale_fill_gradient2(low = "steelblue4", mid = "grey90", high = "red3", midpoint = .2) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave("C://Users/Seth/Desktop/points_heatmap_rev.png", height = 7, width = 10, dpi = 300)



########## GOALIES ##################


### Get the boxscores
boxscore_directory <- "C://Users/Seth/Desktop/clutch media/"
subfolders <- list.files(boxscore_directory)
subfolders <- subfolders[!subfolders == "Graphs"]


boxscore_list_g <- list()
for (i in subfolders) {
  temp_directory_g <- paste0(boxscore_directory, i)
  temp_boxscore_g <- read.csv(paste0(temp_directory_g, "/boxscore_goalie_summary.csv"),  
                            sep = ";")
  temp_boxscore_g$season <- i
  boxscore_list_g[[i]] <- temp_boxscore_g
}

combined_boxscores_g <- do.call(bind_rows, boxscore_list_g)


#filter for regular season only by checking for game IDs in the merged schedule fule
combined_boxscores_g_formatted <- combined_boxscores_g %>%
  filter(Game.Id %in% schedule$gameid) %>%
  mutate(season = as.numeric(as.character(season))) %>%
  select(Game.Id, PlayerId, TeamId, SA, GA, SV, season) %>%
  group_by(season) %>%
  mutate(league_ac_goal_perc = sum(GA)/(sum(SA))) %>%
  mutate(gsaa = (SA*league_ac_goal_perc) - GA) %>%
  left_join(select(opp_vectors, gameid, id, opp), by = c("Game.Id" = "gameid", "TeamId" = "id")) %>%
  left_join(goalie_id_map, by = c("PlayerId" = "id")) %>%
  left_join(select(team_meta, id, abbreviation), by = c("opp" = "id")) %>%
  rename("oppid" = "abbreviation") %>%
  left_join(select(team_meta, id, abbreviation), by = c("TeamId" = "id")) %>%
  rename("team" = "abbreviation")
  
  



#gsaa
gsaa_opp <- combined_boxscores_g_formatted %>%
  group_by(PlayerId, oppid, team) %>%
  summarise(gsaa = sum(gsaa),
            name = Mode(name))

g_team_rank <- gsaa_opp %>%
  group_by(oppid, PlayerId) %>%
  summarise(gsaa = sum(gsaa)) %>%
  mutate(rank = rank(-gsaa, ties.method = "min")) %>%
  select(-gsaa)

gsaa_opp <- gsaa_opp %>%
  left_join(g_team_rank) %>%
  filter(rank <= 3) %>%
  mutate(label_raw = paste0(PlayerId, oppid)) %>%
  ungroup() %>%
  arrange(desc(gsaa)) %>%
  mutate(label_raw = factor(label_raw, levels = unique(label_raw)))


ggplot(gsaa_opp, aes(x = gsaa, y = fct_rev(label_raw), fill = team)) +
  geom_col(col = "black",
           show.legend = F) +
  facet_wrap(.~ oppid, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = team_colors) +
  scale_y_discrete(breaks = gsaa_opp$label_raw, labels = gsaa_opp$name) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = NULL, x = "GSAA", fill = NULL)
ggsave("C://Users/Seth/Desktop/opponent_gsaa.jpg", width = 12, height = 14, dpi = 300)




### goalie diff heatmap

goalie_heatmap_format <- combined_boxscores_g_formatted %>%
  group_by(name, oppid) %>%
  summarise(gsaa = sum(gsaa)) %>%
  group_by(name) %>%
  mutate(mean = mean(gsaa),
         diff = gsaa - mean) %>%
  ungroup() 

goalie_heatmap <- goalie_heatmap_format %>%
  arrange(desc(gsaa)) %>%
  mutate(rank = row_number()) %>%
  group_by(name) %>%
  filter(min(rank) <= 44) %>%
  ungroup() %>%
  select(name, oppid, gsaa, diff) %>%
  mutate(name = factor(name, levels = unique(name)),
         opp = factor(oppid, levels = unique(oppid))) %>%
  distinct()





ggplot(goalie_heatmap, aes(x = opp, y = fct_rev(name), fill = diff)) +
  geom_tile(col = "black",
            show.legend = F) +
  geom_text(aes(label = round(gsaa, 2))) +
  scale_fill_gradient2(low = "steelblue4", mid = "grey90", high = "red3", midpoint = .2) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave("C://Users/Seth/Desktop/goalie_differential.png", height = 8, width = 11, dpi = 300)





goalie_heatmap_rev <- goalie_heatmap_format %>%
  arrange((gsaa)) %>%
  mutate(rank = row_number()) %>%
  group_by(name) %>%
  filter(min(rank) <= 32) %>%
  ungroup() %>%
  select(name, oppid, gsaa, diff) %>%
  mutate(name = factor(name, levels = unique(name)),
         opp = factor(oppid, levels = unique(oppid))) %>%
  distinct()





ggplot(goalie_heatmap_rev, aes(x = opp, y = fct_rev(name), fill = diff)) +
  geom_tile(col = "black",
            show.legend = F) +
  geom_text(aes(label = round(gsaa, 2))) +
  scale_fill_gradient2(low = "steelblue4", mid = "grey90", high = "red3", midpoint = .2) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  labs(x = NULL, y = NULL)
ggsave("C://Users/Seth/Desktop/goalie_differential_rev.png", height = 8, width = 12, dpi = 300)

