### Load libraries
library(tidyverse)
library(httr)
library(jsonlite)

### Load the datasets
seasons <- c(66:83)

team_meta <- index_meta(89)
team_meta_merge <- team_meta %>% select(id, abbreviation)
team_colors <- team_meta$colors_primary
names(team_colors) <- team_meta$abbreviation



### Define a list of violent penalties
violent_pens <- c("Boarding Major", "Charging", "Clipping", "Cross-Checking",
                  "Elbowing", "Fighting", "Illegal CHeck to Head", "Interference Major",
                  "Kneeing", "Misconduct", "Roughing", "Slashing")



### Create a mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




### Create a ggplot theme
theme <- theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())




### Get penalties summary files
penalties_list <- list()
for (i in seasons) {
  print(i)
  temp_penalties <- read.csv(url(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/boxscore_period_penalties_summary.csv")), 
                             sep = ";")
  temp_penalties$season <- i
  penalties_list[[i]] <- temp_penalties
}

all_penalty_summary <- do.call(rbind, penalties_list)



### Get schedules
### To check for regular season game IDs and opponents for each game
schedule_list <- list()
for (i in seasons) {
  print(i)
  temp_sch <- read.csv(url(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/schedules.csv")), 
                       sep = ";")
  schedule_list[[i]] <- temp_sch
}
all_schedules <- do.call(rbind, schedule_list)


### Format schedule for each team and opponent per game id
game_id_map <- all_schedules %>%
  filter(Type == "Regular Season") %>%
  select(Game.Id, Home, Away) %>%
  pivot_longer(cols = -Game.Id,
               names_to = "site",
               values_to = "team") %>%
  arrange(Game.Id, site, team) %>%
  group_by(Game.Id) %>%
  mutate(opp = rev(team)) %>%
  ungroup() %>%
  select(-site)
  

### Get a list of player stats
### To merge names to IDs and for data validation
player_list <- list()
for (i in seasons) {
  print(i)
  player_stats <- GET("http://index.simulationhockey.com/api/v1/players/stats", query = list(season = i))
  player_stats <- fromJSON(rawToChar(player_stats$content))
  player_stats <- do.call(data.frame, player_stats)
  player_list[[i]] <- player_stats
}
combined_player_stats <- do.call(rbind, player_list)

#get just their names and IDs for merging
player_id_map <- combined_player_stats %>%
  select(name, id, season) %>%
  group_by(id, season) %>%
  summarise(name = name[1])




### Get all the boxscores to calculate player GP total and vs. each opp
gp_list <- list()
for (i in seasons) {
  print(i)
  temp_gp <- read.csv(url(paste0("https://simulationhockey.com/games/shl/S", i, "/csv/boxscore_skater_summary.csv")), 
                       sep = ";")
  gp_list[[i]] <- temp_gp
}
all_gp <- do.call(bind_rows, gp_list)


gp_sum <- all_gp %>%
  filter(Game.Id %in% game_id_map$Game.Id) %>%
  left_join(game_id_map, by = c("Game.Id", "TeamId" = "team")) %>%
  group_by(PlayerId, opp) %>%
  summarise(gp_opp = length(unique(Game.Id))) %>%
  group_by(PlayerId) %>%
  mutate(total_gp = sum(gp_opp))
           


### Merge the penalty summary with the game map
merged_penalty_summary <- all_penalty_summary %>%
  filter(Game.Id %in% game_id_map$Game.Id) %>%
  left_join(game_id_map, by = c("Game.Id", "TeamId" = "team")) %>%
  left_join(player_id_map, by = c("season", "Player" = "id"))


### Most PIMS
merged_penalty_summary %>%
  group_by(Player, Penalty) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  group_by(Player) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(Player= factor(Player, levels = unique(Player))) %>%
  mutate(rank = as.numeric(Player)) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  
  ggplot(aes(x = pims, y = fct_rev(name), fill = Penalty)) +
  geom_col(col = "black") +
  theme +
  scale_fill_viridis_d() +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/pims_leader.jpg", width = 13, height = 9, dpi = 300)


### Most penalties
merged_penalty_summary %>%
  group_by(Player, Penalty) %>%
  summarise(pens = n(),
            name = Mode(name)) %>%
  group_by(Player) %>%
  mutate(total_pens = sum(pens)) %>%
  ungroup() %>%
  arrange(desc(total_pens)) %>%
  mutate(Player= factor(Player, levels = unique(Player))) %>%
  mutate(rank = as.numeric(Player)) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  
  ggplot(aes(x = pens, y = fct_rev(name), fill = Penalty)) +
  geom_col(col = "black") +
  theme +
  scale_fill_viridis_d() +
  labs(x = "Penalties", y = NULL)
ggsave("C://Users/Seth/Desktop/pens_leader.jpg", width = 13, height = 9, dpi = 300)


### Most PIMS in a game
game_leaders <- merged_penalty_summary %>%
  group_by(Player, Penalty, Game.Id) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  mutate(unique_id = paste0(Player, Game.Id)) %>%
  group_by(unique_id, Game.Id) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(unique_id= factor(unique_id, levels = unique(unique_id))) %>%
  filter(total_pims > 15) 

ggplot(game_leaders, aes(x = pims, y = fct_rev(unique_id), fill = Penalty)) +
  geom_col(col = "black") +
  theme +
  scale_fill_viridis_d() +
  scale_y_discrete(breaks = game_leaders$unique_id, 
                   labels = game_leaders$name) +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/game_leader.jpg", width = 7, height = 10, dpi = 300)
  


### Most PIMS/penalties in a season
season_leaders <- merged_penalty_summary %>%
  group_by(Player, Penalty, season) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  mutate(unique_id = paste0(Player, season)) %>%
  group_by(unique_id, season) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(unique_id= factor(unique_id, levels = unique(unique_id))) %>%
  mutate(rank = as.numeric(unique_id)) %>%
  filter(rank <= 25) %>%
  mutate(name = paste0(name, " (S", season, ")"))

ggplot(season_leaders, aes(x = pims, y = fct_rev(unique_id), fill = Penalty)) +
  geom_col(col = "black") +
  theme +
  scale_fill_viridis_d() +
  scale_y_discrete(breaks = season_leaders$unique_id, 
                   labels = season_leaders$name) +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/season_leader.jpg", width = 13, height = 9, dpi = 300)


### NHL PIMs by tpye
nhl_pims <- read_csv("C://Users/Seth/Desktop/nhl_pens.csv")

nhl_pims %>%
  mutate(Penalty = factor(`Penalty Type`, levels = `Penalty Type`)) %>%
  
  ggplot(aes(x = Count, y = fct_rev(Penalty), fill = Count)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "Penalty counts", y = NULL)
ggsave("C://Users/Seth/Desktop/nhl_pims_by_pim.jpg", width = 10, height = 12, dpi = 300)


### Most PIMs by type
pims_by_type <- merged_penalty_summary %>%
  filter(Penalty != "Bench") %>%
  group_by(Player, Penalty) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  mutate(unique_id = paste0(name, Penalty)) %>%
  ungroup() %>%
  arrange(Penalty, desc(pims)) %>%
  group_by(Penalty) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5) %>%
  mutate(unique_id = factor(unique_id, levels = unique_id))

ggplot(pims_by_type, aes(x = pims, y = fct_rev(unique_id), fill = pims)) +
  geom_col(col = "black",
           show.legend = F) +
  facet_wrap(.~ Penalty, scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank()) +
  scale_y_discrete(breaks = pims_by_type$unique_id,
                   labels = pims_by_type$name) +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/leads_by_type.jpg", width = 24, height = 16, dpi = 300)

  

### Most 'violent' based on certain types of penalites
merged_penalty_summary %>%
  filter(Penalty %in% violent_pens) %>%
  mutate(Minutes = case_when(Penalty == "Slashing" ~ Minutes*.5,
                             TRUE ~ Minutes)) %>%
  group_by(Player, Penalty) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  group_by(Player) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(Player= factor(Player, levels = unique(Player))) %>%
  mutate(rank = as.numeric(Player)) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  
  ggplot(aes(x = pims, y = fct_rev(name), fill = Penalty)) +
  geom_col(col = "black") +
  theme +
  scale_fill_viridis_d() +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/violent_pims_leader.jpg", width = 13, height = 9, dpi = 300)


  

### Breakdown of all type of PIM instances
penalty_type_leaders <- merged_penalty_summary %>%
  group_by(Penalty) %>%
  summarise(mins = sum(Minutes),
            penalties = n())


penalty_type_leaders %>%
  arrange(desc(mins)) %>%
  mutate(Penalty = factor(Penalty, levels = Penalty)) %>%
  
  ggplot(aes(x = mins, y = fct_rev(Penalty), fill = mins)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/pims_by_pim.jpg", width = 10, height = 8.5, dpi = 300)




penalty_type_leaders %>%
  arrange(desc(penalties)) %>%
  mutate(Penalty = factor(Penalty, levels = Penalty)) %>%
  
  ggplot(aes(x = penalties, y = fct_rev(Penalty), fill = penalties)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "Total penalties", y = NULL)
ggsave("C://Users/Seth/Desktop/pims_by_n.jpg", width = 10, height = 8.5, dpi = 300)


### Random follow up, most served bench minors
merged_penalty_summary %>%
  filter(Penalty == "Bench") %>% 
  group_by(Player) %>% 
  summarise(n = n(), 
            name = Mode(name)) %>% 
  arrange(desc(n)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  filter(n >= 10) %>%
  
  ggplot(aes(x = n, y = fct_rev(name), fill = n)) +
  geom_col(col = "black") +
  theme +
  scale_fill_gradient(low = "white", high = "dodgerblue4") +
  labs(x = "Bench minors served", y = NULL)
ggsave("C://Users/Seth/Desktop/bench_minor_served.jpg", width = 10, height = 8.5, dpi = 300)


### Most common fight opponent
fighting <- merged_penalty_summary %>%
  filter(Penalty == "Fighting") %>%
  group_by(Game.Id, Period, Time) %>%
  mutate(opp_name = rev(name)) %>%
  group_by(name, opp_name) %>%
  summarise(n = n())


fighting_opps <- merged_penalty_summary %>%
  filter(Penalty == "Fighting") %>%
  group_by(TeamId, opp) %>%
  summarise(n = n()) %>%
  left_join(select(team_meta, id, abbreviation), by = c("TeamId" = "id")) %>%
  left_join(select(team_meta, id, abbreviation), by = c("opp" = "id"))
  
fighting_origin <- fighting_opps$abbreviation.x
fighting_dest <- fighting_opps$abbreviation.y
data <- data.frame(fighting_origin, fighting_dest)


# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(fighting_origin, fighting_dest))

chordDiagram(adjacencyData, transparency = .5, symmetric = F, grid.col = team_colors)


### Most PIMS/penalties against each opponent
  ### Can break it down by leaders, type, per game, etc
per_team <- merged_penalty_summary %>%
  group_by(Player, opp) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  group_by(Player) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(Player= factor(Player, levels = unique(Player))) %>%
  mutate(rank = as.numeric(Player)) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = unique(name))) 


per_team_leaders <- per_team %>%
  arrange(opp, desc(pims)) %>%
  group_by(opp) %>%
  mutate(rank = row_number(),
         unique_id = paste0(opp, name)) %>%
  ungroup() %>%
  filter(rank <= 3) %>%
  mutate(unique_id = factor(unique_id, levels = unique(unique_id))) %>%
  left_join(select(team_meta, id, abbreviation), by = c("opp" = "id"))

ggplot(per_team_leaders, aes(x= pims, y = fct_rev(unique_id), fill = pims)) +
  geom_col(col = "black",
           show.legend = F) +
  facet_wrap(.~ abbreviation, scales = "free_y") +
  scale_y_discrete(breaks = per_team_leaders$unique_id,
                   labels = per_team_leaders$name) +
  theme +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/per_team_pims.jpg", width = 16.5, height =  12.5, dpi = 300)






### Most violent PIMS/penalties against each opponent
### Can break it down by leaders, type, per game, etc
violent_per_team <- merged_penalty_summary %>%
  filter(Penalty %in% violent_pens) %>%
  group_by(Player, opp) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  group_by(Player) %>%
  mutate(total_pims = sum(pims)) %>%
  ungroup() %>%
  arrange(desc(total_pims)) %>%
  mutate(Player= factor(Player, levels = unique(Player))) %>%
  mutate(rank = as.numeric(Player)) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = unique(name))) 


violent_per_team_leaders <- violent_per_team %>%
  arrange(opp, desc(pims)) %>%
  group_by(opp) %>%
  mutate(rank = row_number(),
         unique_id = paste0(opp, name)) %>%
  ungroup() %>%
  filter(rank <= 3) %>%
  mutate(unique_id = factor(unique_id, levels = unique(unique_id))) %>%
  left_join(select(team_meta, id, abbreviation), by = c("opp" = "id"))

ggplot(violent_per_team_leaders, aes(x= pims, y = fct_rev(unique_id), fill = pims)) +
  geom_col(col = "black",
           show.legend = F) +
  facet_wrap(.~ abbreviation, scales = "free_y") +
  scale_y_discrete(breaks = violent_per_team_leaders$unique_id,
                   labels = violent_per_team_leaders$name) +
  theme +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/violent_per_team_pims.jpg", width = 16.5, height =  12.5, dpi = 300)

  
### Most games where they got a penalty (not just racking up a bunch in one game)
games_with_pen <- merged_penalty_summary %>%
  group_by(Player) %>%
  summarise(name = Mode(name),
            pen_games = length(unique(Game.Id))) %>%
  arrange(desc(pen_games)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = name))


ggplot(games_with_pen, aes(x = pen_games, y = fct_rev(name), fill = fct_rev(factor(pen_games)))) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_viridis_d() +
  labs(x = "Games with a penalty", y = NULL)
ggsave("C://Users/Seth/Desktop/game_pen.jpg", width = 13, height = 9, dpi = 300)



### Most games where they got multiple penalties (not just racking up a bunch in one game)
multi_pen_games <- merged_penalty_summary %>%
  group_by(Player, Game.Id) %>%
  summarise(name = Mode(name),
            n = n()) 


### Lady byng
lady_byng <- merged_penalty_summary %>%
  group_by(Player) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  left_join(gp_sum, by = c("Player" = "PlayerId")) %>%
  mutate(pim_gp = pims/total_gp) %>%
  filter(total_gp >= 198) %>%
  group_by(Player) %>%
  summarise(name = name[1],
            pim_gp = pim_gp[1]) %>%
  arrange(pim_gp) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = name))
  
ggplot(lady_byng, aes(x = pim_gp, y = fct_rev(name), fill = pim_gp)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "PIMs per GP", y = NULL)
ggsave("C://Users/Seth/Desktop/lady_byng.jpg", width = 13, height = 9, dpi = 300)






### Lady byng for a season
lady_byng_season <- combined_player_stats %>%
  filter(gamesPlayed >= 66) %>%
  arrange(pim) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = name))

ggplot(lady_byng_season, aes(x = pim, y = fct_rev(name), fill = pim)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "Single season PIMs", y = NULL)
ggsave("C://Users/Seth/Desktop/lady_byng_season.jpg", width = 13, height = 9, dpi = 300)




### most per game
anti_lady_byng <- merged_penalty_summary %>%
  group_by(Player) %>%
  summarise(pims = sum(Minutes),
            name = Mode(name)) %>%
  left_join(gp_sum, by = c("Player" = "PlayerId")) %>%
  mutate(pim_gp = pims/total_gp) %>%
  filter(total_gp >= 198) %>%
  group_by(Player) %>%
  summarise(name = name[1],
            pim_gp = pim_gp[1]) %>%
  arrange(desc(pim_gp)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 25) %>%
  mutate(name = factor(name, levels = name))

ggplot(anti_lady_byng, aes(x = pim_gp, y = fct_rev(name), fill = pim_gp)) +
  geom_col(col = "black",
           show.legend = F) +
  theme +
  scale_fill_gradient(low = "#01F38E", high = "#0101FC") +
  labs(x = "PIMs per GP", y = NULL)
ggsave("C://Users/Seth/Desktop/anti_lady_byng.jpg", width = 13, height = 9, dpi = 300)

