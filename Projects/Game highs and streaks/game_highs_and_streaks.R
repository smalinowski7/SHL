### Set variables
season <- 85


### Load libraries and dataframes
source("scraper_functions.R")
library(here)
library(tidyverse)


### Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


### Streaks function
myrleid <- function(x) {
  rl <- rle(x)$lengths
  rep(seq_along(rl), times = rl)
}


### Load boxscores
boxscores <- file_boxscores(85, append = TRUE)
goalie_boxscores <- file_goalie_summary(85, append = TRUE)


### Load schedule and filter for S66 and above 
schedule <- index_schedule(85, append = TRUE)
schedule <- schedule %>%
  arrange(date) %>%
  filter(season >= 66)


### Create player mapping from ratings
meta <- index_player_ratings(season, append = TRUE) 
meta <- meta %>%
  select(id, name, season)


### Create goalie mapping from ratings
g_meta <- index_goalie_ratings(season, append = TRUE)
g_meta <- g_meta %>%
  select(id, name, season)


### Load team meta
team_meta <- index_meta(season, append = TRUE)


### Set team colors
team_colors <- team_meta$colors_primary
names(team_colors) <- team_meta$abbreviation


### Filter for regular season games only
boxscores_rg <- boxscores %>% 
  filter(Game.Id %in% schedule$gameid) %>%
  left_join(meta, by = c("PlayerId" = "id", "season")) %>%
  left_join(select(team_meta, id, season, abbreviation),
            by = c("TeamId" = "id", "season")) %>%
  mutate(P = G+A) %>%
  mutate(Game.Id = factor(Game.Id, levels = schedule$gameid)) %>%
  arrange(Game.Id)


### Filter goalie boxscore for regular season games only
goalie_boxscores_rg <- goalie_boxscores %>% 
  filter(TOI > 0) %>%
  filter(Game.Id %in% schedule$gameid) %>%
  left_join(g_meta, by = c("PlayerId" = "id", "season")) %>%
  left_join(select(team_meta, id, season, abbreviation),
            by = c("TeamId" = "id", "season")) %>%
  group_by(season) %>%
  mutate(league_av_sh = sum(GA)/sum(SA)) %>%
  ungroup() %>%
  mutate(avg_ga = SA*league_av_sh,
         GSAA = avg_ga - GA) %>%
  mutate(Game.Id = factor(Game.Id, levels = schedule$gameid)) %>%
  arrange(Game.Id)


#########################
### Single game highs ###
#########################

### Pick a stat here and calculate the top single game leaders
### Stat should be how it appears on the boxscores

stat <- "TK"
plot_filter <- 7

sg_stat_leaders <- boxscores_rg %>%
  arrange(desc(get(stat))) %>%
  select(season, abbreviation, name, all_of(stat), everything())


sg_plot <- sg_stat_leaders %>%
  
  filter(get(stat) >= plot_filter) %>%
  
  mutate(unique_id = paste0(name, Game.Id)) %>%
  mutate(label = paste0(name, " (S", season, ")")) %>%
  mutate(unique_id = factor(unique_id, levels = unique_id))

ggplot(sg_plot, aes(x = .data[[stat]], y = fct_rev(unique_id))) +
  geom_col(aes(fill = abbreviation),
           col = "black",
           show.legend = F) +
  scale_y_discrete(breaks = sg_plot$unique_id, 
                   labels = sg_plot$label) +
  scale_fill_manual(values = team_colors) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(title = paste0("Single game ", stat, " high"), y = NULL, x ="Takeaways")

ggsave(here("Projects/Game highs and streaks/plots/single_game_tk.png"),height = 9, width = 7, dpi = 300)


##########################
### Single game multis ###
##########################

### Pick a stat here and calculate the number of mulit-stat games
### Stat should be how it appears on the boxscores

stat <- "TK"

mg_stat_leaders <- boxscores_rg %>%
  mutate(multi_stat = ifelse(get(stat) > 1, 1, 0)) %>%
  filter(multi_stat == 1) %>%
  group_by(PlayerId, abbreviation, get(stat)) %>%
  summarise(name = Mode(name),
            
            n = sum(multi_stat)) %>%
  arrange(desc(n)) %>%
  select(name, n, everything())

mg_stat_plot <- mg_stat_leaders %>%
  group_by(name) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  mutate(name = paste0(name, " (", total, ")")) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  mutate(rank = as.numeric(name)) %>%
  group_by(`get(stat)`) %>%
  mutate(max = (n == max(n)),
         final_label = paste0("#", rank, ". ", name),
         final_label = factor(final_label, levels = unique(final_label))) %>%
  
  
  filter(rank <= 15) 


ggplot(mg_stat_plot, aes(x = `get(stat)`, y = n, fill = abbreviation)) +
  geom_col(col = "black",
           show.legend = F) +
  facet_wrap(.~ final_label, ncol = 1) +
  scale_fill_manual(values = team_colors) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank()) +
  labs(x = paste0("Single game ", stat, "s"), y = "Number of games", fill = "Team") +
  
  geom_text(data = filter(mg_stat_plot, max == TRUE), aes(x = `get(stat)`, y = n*1.05),
            label = "*",
            size = 6) +
  ylim(c(0, max(mg_stat_plot$n)*1.25))

ggsave(here("Projects/Game highs and streaks/plots/multi_tk.png"), height = 15, width = 7, dpi = 300)




####################
### Stat streaks ###
####################

### Pick a stat here and calculate the streak number for players
### Stat should be how it appears on the boxscores

stat <- "BS"

streaks <- boxscores_rg %>%
  mutate(streak_stat = ifelse(get(stat) > 0, 1, 0)) %>%
  group_by(PlayerId) %>%
  mutate(steak_id = cumsum(c(1, diff(streak_stat) != 0))) %>% 
  ungroup()

stat_streaks <- streaks %>%
  filter(streak_stat == 1) %>%
  group_by(PlayerId, steak_id) %>%
  summarise(
    season_start = min(season),
    season_end = max(season),
    team = paste0(unique(abbreviation), collapse = ","),
    name = Mode(name),
    streak_length = n()
    
  ) %>%
  ungroup() %>%
  arrange(desc(streak_length)) %>%
  mutate(rank = row_number()) 


streak_plot <- stat_streaks %>%
  ungroup() %>%
  arrange(desc(streak_length)) %>%
  mutate(label = case_when(season_start == season_end ~ paste0(name, " (S", season_start, ")"),
                           TRUE ~ paste0(name, " (S", season_start, "-S", season_end, ")"))) %>%
  mutate(plot_id = paste0(label, steak_id)) %>%
  mutate(plot_id = factor(plot_id, levels = unique(plot_id))) %>%
  filter(rank <= 20)



ggplot(streak_plot, aes(x = streak_length, y = fct_rev(plot_id), fill = streak_length)) +
  geom_col(col = "black",
           show.legend = F,
           width = .85) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  scale_y_discrete(breaks = streak_plot$plot_id, 
                   labels = streak_plot$label) +
  scale_fill_gradient(low = "grey85", high = "#52485B") +
  labs(x = "Streak length", y = NULL, title = "SHL shot block streaks")

ggsave("Projects/Game highs and streaks/plots/streak_bs.png", 
       width = 6.5,
       height = 7.5,
       dpi = 300)



################################
### Goalie single game highs ###
################################

### Pick a stat here and calculate the top single game leaders
### Stat should be how it appears on the boxscores

stat <- "GSAA"

goalie_sg_stat_leaders <- goalie_boxscores_rg %>%
  arrange(desc(get(stat))) %>%
  select(season, abbreviation, name, all_of(stat), everything())


goalie_sg_plot <- goalie_sg_stat_leaders %>%
  
 mutate(rank = row_number()) %>%
  filter(rank <= 20) %>%
  
  mutate(unique_id = paste0(name, Game.Id)) %>%
  mutate(label = paste0(name, " (S", season, ")")) %>%
  mutate(unique_id = factor(unique_id, levels = unique_id))

ggplot(goalie_sg_plot, aes(x = .data[[stat]], y = fct_rev(unique_id))) +
  geom_col(aes(fill = abbreviation),
           col = "black",
           show.legend = F) +
  scale_y_discrete(breaks = goalie_sg_plot$unique_id, 
                   labels = goalie_sg_plot$label) +
  scale_fill_manual(values = team_colors) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  labs(title = paste0("Single game ", stat, " high"), y = NULL, x = "GSAA")

ggsave(here("Projects/Game highs and streaks/plots/goalie_single_game_gsaa.png"),
       width = 6.5,
       height = 7.5,
       dpi = 300)






###########################
### Goalie Stat streaks ###
###########################

### Pick a stat here and calculate the streak number for players
### Stat should be how it appears on the boxscores

stat <- "SV"

goalie_streaks <- goalie_boxscores_rg %>%
  mutate(streak_stat = ifelse(get(stat) > 29, 1, 0)) %>%
  group_by(PlayerId) %>%
  mutate(steak_id = cumsum(c(1, diff(streak_stat) != 0))) %>% 
  ungroup()

goalie_stat_streaks <- goalie_streaks %>%
  filter(streak_stat == 1) %>%
  group_by(PlayerId, steak_id) %>%
  summarise(
    season_start = min(season),
    season_end = max(season),
    team = paste0(unique(abbreviation), collapse = ","),
    name = Mode(name),
    streak_length = n()
    
  ) %>%
  ungroup() %>%
  arrange(desc(streak_length)) %>%
  mutate(rank = row_number()) 


goalie_streak_plot <- goalie_stat_streaks %>%
  ungroup() %>%
  arrange(desc(streak_length)) %>%
  mutate(label = case_when(season_start == season_end ~ paste0(name, " (S", season_start, ")"),
                           TRUE ~ paste0(name, " (S", season_start, "-S", season_end, ")"))) %>%
  mutate(plot_id = paste0(label, steak_id)) %>%
  mutate(plot_id = factor(plot_id, levels = unique(plot_id))) %>%
  filter(rank <= 20)



ggplot(goalie_streak_plot, aes(x = streak_length, y = fct_rev(plot_id), fill = streak_length)) +
  geom_col(col = "black",
           show.legend = F,
           width = .85) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  scale_y_discrete(breaks = goalie_streak_plot$plot_id, 
                   labels = goalie_streak_plot$label) +
  scale_fill_gradient(low = "grey85", high = "red3") +
  labs(x = "Streak length", y = NULL, title = "SHL 30+ save streaks")

ggsave("Projects/Game highs and streaks/plots/goalie_streak_sv.png", 
       width = 6.5,
       height = 7.5,
       dpi = 300)


