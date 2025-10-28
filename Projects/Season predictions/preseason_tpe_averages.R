### Index needs to be up to date in order to use this
source("scraper_functions.R")
library(tidyverse)
library(jsonlite)
library(httr)

season <- 84


### Scrape player and goalie ratings
player_ratings <- index_player_ratings(seasons = season,
                                       league = 2)
  

goalie_ratings <- index_goalie_ratings(seasons = season,
                                       league = 2)


### Average team TPE by positional group
skater_tpe <- player_ratings %>%
  group_by(team, pos) %>%
  summarise(tpe = mean(appliedTPE))


goalie_tpe <- goalie_ratings %>%
  group_by(team, pos) %>%
  summarise(tpe = mean(appliedTPE)) 


### Bind skater averages with goalie averages and calculate overall TPE
team_tpe <- skater_tpe %>%
  rbind(goalie_tpe) %>%
  pivot_wider(names_from = pos,
              values_from = tpe) %>%
  mutate(Overall = ((12*Forward) + (6*Defense) + (2*Goalie))/20) %>%
  mutate(Skaters = ((12*Forward) + (6*Defense))/18) %>%
  select(team, Forward, Defense, Goalie, Skaters, Overall) %>%
  arrange(desc(Skaters))


### Look at potentially some of the top players in the league by tpe
player_tpe_rank <- player_ratings %>%
  select(team, appliedTPE, pos, everything()) %>%
  arrange(desc(appliedTPE))
  