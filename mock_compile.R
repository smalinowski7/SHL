source("scraper_functions.R")
library(jsonlite)
library(httr)
library(tidyverse)

### Enter the current draft season of the rookies
rookie_season <- 85
  

### Load the mock responses
mock_responses <- read_csv("Data/Misc/mock_draft_responses.csv")


### Scrape portal for rookie usernames  
player_list <- portal_players()

rookie_usernames <- player_list$username[player_list$draftSeason == rookie_season]



### Format the mock to long and filter for only rookie options
mock_response_long <- mock_responses %>%
  pivot_longer(cols = c(-Username, -`Verification word`),
               names_to = "pick",
               values_to = "player") %>%
  filter(Username %in% rookie_usernames)%>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  mutate(player = factor(player, levels = player))



### Plot the rookie answers
ggplot(mock_response_long, aes(x = n, y = player, fill = n)) +
  geom_col(col = "black",
           show.legend = F) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = nrow(mock_response_long) - 9.5, linetype = "dashed") +
  scale_fill_gradient(low = "#D4EDF2", high = "#22718F") +
  labs(y = NULL, x = "Number of top 10 mocks")




### Repeat process for entire league
mock_response_long <- mock_responses %>%
  pivot_longer(cols = c(-Username, -`Verification word`),
               names_to = "pick",
               values_to = "player") %>%
  filter(!is.na(player)) %>%
  group_by(player) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  mutate(player = factor(player, levels = player))



### Plot entire league's response
ggplot(mock_response_long, aes(x = n, y = player, fill = n)) +
  geom_col(col = "black",
           show.legend = F) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = nrow(mock_response_long) - 9.5, linetype = "dashed") +
  scale_fill_gradient(low = "#D4EDF2", high = "#22718F") +
  labs(y = NULL, x = "Number of top 10 mocks")

