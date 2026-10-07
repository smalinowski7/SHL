library(tidyverse)
library(here)

data <- read_csv(here("Projects/GM stats/gm.csv"))


data_formatted <- data %>%
  mutate(gm_lower = tolower(gm),
         seasons = (end-start)+1) %>%
  group_by(gm_lower) %>%
  summarise(teams = paste0(team, collapse = ","),
            tot_wins = sum(wins),
            tot_losses = sum(losses),
            tot_otl = sum(otl),
            tot_seasons = sum(seasons),
            tot_games = tot_wins + tot_losses + tot_otl,
            win_perc = tot_wins/tot_games) %>%
  select(gm_lower, win_perc, everything())



one_team_gms <- data %>%
  group_by(gm) %>%
  filter(n() == 1)


data_formatted_one_gm <- data_formatted %>%
  filter(gm_lower %in% tolower(one_team_gms$gm))
