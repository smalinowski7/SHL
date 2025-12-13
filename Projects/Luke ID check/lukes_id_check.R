data <- read.csv("Projects/Luke ID check/player_master.csv")


data_formatted <- data %>%
  mutate(name_fix = case_when(First.Name %in% c("", " ", "  ", " .", ".", ". ") ~ Last.Name,
                              TRUE ~ paste0(First.Name, " ", Last.Name)))


### Multiple IDs for the same name
league <- 3

name_test <- data_formatted %>%
  filter(TeamID != -1) %>%
  filter(LeagueID == league) %>%
  group_by(name_fix) %>%
  mutate(unique_ids = length(unique(PlayerID))) %>%
  filter(unique_ids > 1) %>%
  group_by(name_fix, PlayerID) %>%
  summarise(seasons = paste0(SeasonID, collapse = ", "))

write_csv(name_test, "Projects/Luke ID check/WJC_multiple_IDs.csv")


### Multiple names for the same ID
league <- 0

name_test_2 <- data_formatted %>%
  filter(TeamID != -1) %>%
  filter(LeagueID == league) %>%
  group_by(PlayerID) %>%
  mutate(unique_names = length(unique(name_fix))) %>%
  filter(unique_names > 1) %>%
  group_by(name_fix, PlayerID) %>%
  summarise(seasons = paste0(SeasonID, collapse = ", ")) %>%
  arrange(PlayerID)

write_csv(name_test_2, "Projects/Luke ID check/SHL_multiple_names.csv")



### For the WJF, load career stats to filter for only IDs that have actually played in the WJC
league <- 3

player_stats <- read_csv("Projects/Luke ID check/player_skater_stats_rs.csv")
goalie_stats <- read_csv("Projects/Luke ID check/player_goalie_stats_rs.csv")

name_test <- data_formatted %>%
  filter(TeamID != -1) %>%
  filter(LeagueID == league) %>%
  filter(PlayerID %in% player_stats$PlayerID | PlayerID %in% goalie_stats$PlayerID)
  group_by(name_fix) %>%
  mutate(unique_ids = length(unique(PlayerID))) %>%
  filter(unique_ids > 1) %>%
  group_by(name_fix, PlayerID) %>%
  summarise(seasons = paste0(SeasonID, collapse = ", "))