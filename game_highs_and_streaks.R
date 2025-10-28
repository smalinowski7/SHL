### Load libraries and dataframes
source("scraper_functions.R")
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


boxscores <- read_csv("Data/SHL/file_boxscore.csv")

schedule <- read_csv("Data/SHL/index_schedule.csv")
schedule <- schedule %>%
  arrange(date) %>%
  filter(season >= 66)

meta <- read_csv("Data/SHL/index_player_ratings.csv") %>%
  select(id, name, season)

team_meta <- read_csv("Data/SHL/index_meta.csv")

### Filter for regular season games only
boxscores_rg <- boxscores %>% 
  filter(Game.Id %in% schedule$gameid) %>%
  left_join(meta, by = c("PlayerId" = "id", "season")) %>%
  left_join(select(team_meta, id, season, abbreviation),
            by = c("TeamId" = "id", "season")) %>%
  mutate(P = G+A) %>%
  mutate(Game.Id = factor(Game.Id, levels = schedule$gameid)) %>%
  arrange(Game.Id)


#########################
### Single game highs ###
#########################

### Pick a stat here and calculate the top single game leaders
### Stat should be how it appears on the boxscores

stat <- "BS"

sg_stat_leaders <- boxscores_rg %>%
  arrange(desc(get(stat))) %>%
  select(season, abbreviation, name, all_of(stat), everything())


##########################
### Single game multis ###
##########################

### Pick a stat here and calculate the number of mulit-stat games
### Stat should be how it appears on the boxscores

stat <- "BS"

mg_stat_leaders <- boxscores_rg %>%
  mutate(multi_stat = ifelse(get(stat) > 4, 1, 0)) %>%
  filter(multi_stat == 1) %>%
  group_by(PlayerId) %>%
  summarise(name = Mode(name),
            team = paste0(unique(abbreviation), collapse = ","),
            n = sum(multi_stat)) %>%
  arrange(desc(n)) %>%
  select(name, n, everything())


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
  arrange(desc(streak_length))

