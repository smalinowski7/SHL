### Load libraries 
library(tidyverse)
library(httr)
library(jsonlite)



### Load datasets ###

# Load portal
portal_url <- GET("https://portal.simulationhockey.com/api/v1/player")
portal <- fromJSON(rawToChar(portal_url$content))

# Load events file
events_url <- GET("https://portal.simulationhockey.com/api/v1/updateevents")
event_history <- fromJSON(rawToChar(events_url$content))

# Load SHL team meta
shl_teams_url <- GET("https://index.simulationhockey.com/api/v1/teams")
teams <- fromJSON(rawToChar(shl_teams_url$content))
team_names <- teams$name

# Load SMJHL team meta
j_teams_url <- GET("https://index.simulationhockey.com/api/v1/teams", query = list(league = 1))
j_teams <- fromJSON(rawToChar(j_teams_url$content))
j_team_names <- c(j_teams$name, "Regina Elk")

# Load draft data
draft_url <- GET("https://portal.simulationhockey.com/api/v1/history/draft")
draft_data <- fromJSON(rawToChar(draft_url$content))

# Get lists of teams SHL players were drafted to
shl_draft_data <- draft_data %>%
  filter(leagueID == 0) %>%
  left_join(teams, by = c("teamID" = "id")) %>%
  select(playerUpdateID, name) %>%
  rename("draft_team" = "name")

# Filter and format expansion draft players and teams
s83_expansion <- draft_data %>%
  filter(isExpansion == TRUE & seasonID == 83)
expansion_teams <- c("Cincinnati Six", "Denver Glacier Guardians", "Madison Valkyries", "Nashville Sound")

# Filter the portal for current players
current_players <- portal %>%
  filter(currentLeague == "SHL")


# Format and process transactions data 
transactions <- event_history %>% 
  filter(attributeChanged %in% c("currentLeague", "shlRightsTeamID", "currentTeamID")) %>%
  mutate(newValue = case_when(newValue == "Denver Glacial Guardians" ~ "Denver Glacier Guardians",
                              TRUE ~ newValue)) %>%
  separate(eventDate, into = c("eventDate", "extra"), sep = "T") %>%
  select(-extra) %>%
  mutate(eventDate = as.Date(eventDate))


# (Attempt to) classify each player movement event
player_movement <- transactions %>%
  left_join(shl_draft_data) %>%
  arrange(playerName, eventID) %>%


  group_by(playerName, attributeChanged) %>%
  mutate(event_num = row_number()) %>%
  group_by(playerName) %>%

  filter(oldValue %in% team_names | newValue %in% team_names) %>%
  
  mutate(event_type = case_when(
                                (newValue == draft_team & (length(unique(newValue[newValue != ""])) == 1)) ~ "Drafted",
                                (oldValue != "" & newValue %in% expansion_teams & playerName %in% s83_expansion$playerName & eventDate <= "2025-06-10") ~ "Expansion draft",
                                (oldValue == "" & newValue %in% team_names & attributeChanged == "shlRightsTeamID" & event_num == 1) ~ "Drafted",
                                (oldValue %in% j_team_names & newValue %in% team_names) ~ "call-up",
                                (oldValue %in% team_names & newValue %in% j_team_names) ~ "Send down",
                                (oldValue == "" & newValue %in% team_names) ~ "FA signing",
                                (oldValue %in% team_names & newValue == "") ~ "Left to FA",
                                (oldValue %in% team_names & newValue %in% team_names) ~ "Trade",
                                newValue == lag(oldValue) ~ "re-sign")) %>%
  

  mutate(event_type2 = case_when((event_type == "FA signing" & (newValue == lag(oldValue) | newValue == lag(newValue))) ~ "re-sign",
                                TRUE ~ event_type)) %>%
  
  
  filter(!(event_type2 %in% c("re-sign", "Left to FA", "Send down", "call-up")))


# Filter the most recent player movement event for each player
last_player_movement <- player_movement %>%
  group_by(playerUpdateID) %>%
  mutate(event_num_total = row_number()) %>%
  filter(event_num_total == max(event_num_total)) %>%
  ungroup()


# Using current players, assign each team's roster to their transaction classifier
current_players_short <- current_players %>%
  select(name, currentTeamID) %>%
  filter(!(is.na(currentTeamID))) %>%
  left_join(select(last_player_movement, playerName, oldValue, newValue, event_type2), by = c("name" = "playerName")) %>%
  left_join(select(teams, id, abbreviation, name), by = c("currentTeamID" = "id")) %>%
  left_join(select(teams, abbreviation, name), by = c("oldValue" = "name")) %>%
  
  mutate(plot_team = case_when(event_type2 == "Drafted" ~ abbreviation.x,
                               event_type2 == "Trade" ~ abbreviation.y,
                               event_type2 == "FA signing" ~ "FA",
                               event_type2 == "Expansion draft" ~ abbreviation.y)) 

colors <- c(teams2$colors.primary, "grey75")
names(colors) <- c(teams2$abbreviation, "FA")         



# Plot
plot_teams <- current_players_short %>%
  filter(name.y == "Edmonton Blizzard") %>%
  mutate(row = c(1,2,3,5,6,7,9,10,11,1.5,2.5,5.5,6.5,9.5,10.5,5.5,6.5),
         column = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3))


ggplot(plot_teams, aes(x = row, y = column)) +
  geom_point(aes(fill = plot_team),
             shape = 21,
             col = "black",
             size = 15,
             show.legend = F) +
  geom_text(aes(label = plot_team),
            col = ifelse(plot_teams$plot_team %in% c("CIN", "FA"), "black", "white")) +
  geom_text(aes(y = column - .2, 
                label = name.x),
            size = 3) +
  theme_void() +
  scale_fill_manual(values = colors) +
  scale_y_reverse()


ggplot(colorsdf, aes(x = x, y = y, fill = name)) +
  geom_point(shape = 21, size = 15) +
  geom_text(aes(label = name), col = "white") +
  scale_fill_manual(values = colors)
