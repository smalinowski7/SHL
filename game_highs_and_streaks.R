### Load libraries and dataframes
source("scraper_functions.R")
library(tidyverse)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

boxscores <- read_csv("Data/SHL/file_boxscore.csv")

schedule <- read_csv("Data/SHL/index_schedule.csv")

meta <- read_csv("Data/SHL/index_player_ratings.csv") %>%
  select(id, name, season)

team_meta <- read_csv("Data/SHL/index_meta.csv")

### Filter for regular season games only
boxscores_rg <- boxscores %>% 
  filter(Game.Id %in% schedule$gameid) %>%
  left_join(meta, by = c("PlayerId" = "id", "season")) %>%
  left_join(select(team_meta, id, season, abbreviation),
            by = c("TeamId" = "id", "season")) %>%
  mutate(P = G+A)


#########################
### Single game highs ###
#########################

### Pick a stat here and calculate the top single game leaders
### Stat should be how it appears on the boxscores

stat <- "TK"

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
  mutate(multi_stat = ifelse(get(stat) > 1, 1, 0)) %>%
  filter(multi_stat == 1) %>%
  group_by(PlayerId) %>%
  summarise(name = Mode(name),
            team = Mode(abbreviation),
            n = sum(multi_stat)) %>%
  arrange(desc(n)) %>%
  select(name, n, everything())
