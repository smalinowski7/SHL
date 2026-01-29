source("scraper_functions.R")

# Load libraries
library(tidyverse)



# Load data
player_stats <- read_csv("Data/SHL/index_player_stats.csv")


# Format player stats 
player_stats_formatted <- player_stats %>%
  select(id, name, season, team, pos, pos_broad, gamesPlayed, points, goals, assists) 


# Long version as well
player_stats_long <- player_stats_formatted %>%
  pivot_longer(cols = c(goals, assists, points),
               names_to = "stat",
               values_to = "value") 
  


# Check for normality
qq <- player_stats_long %>%
  group_by(season, stat) %>%
  summarise(qq = qqnorm(value, plot.it = FALSE)) %>%
  group_by(season, stat) %>%
  summarise(r2 = cor(qq$x, qq$y)^2)
  

################
### QQ plots ###
################


for ( i in unique(player_stats_long$pos_broad)) {
  for (j in unique(player_stats_long$stat)) {
    
    print(paste(i, j))
    
    plot_df <- player_stats_long %>%
      filter(stat == j, pos_broad == i)
  
    
    print(
      plot_df %>%
        ggplot(aes(sample = value)) +
        stat_qq() +
        stat_qq_line() + # Adds a line through the quartiles
        facet_wrap(~ season, scales = "free_y") +
        labs(title = (paste(i, j)))
    )
  }
}
      
        



################
### Boxplots ###
################


# Random sample for justification
random_seasons <- c(53, 60, 66, 74, 84)

just_plot <- player_stats_long %>%
  filter(season %in% random_seasons)


ggplot(just_plot %>% filter(stat == "goals"), aes(x = season, y = value, group = season)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = .1) +
  facet_wrap(.~ pos_broad, nrow = 1)




ggplot(player_stats_formatted, aes(x = season, y = goals, group = season)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(aes(fill = factor(season)),
              width = .1,
              alpha = .5,
              shape = 21,
              col = "black")


################
### z scores ###
################

z_scores <- player_stats_long %>%
  group_by(season, stat, pos_broad) %>%
  mutate(z_score = (value - mean(value))/sd(value)) %>%
  ungroup()



# Redo just plot with zscores now
just_plot_with_z <- z_scores %>%
  filter(season %in% random_seasons)


ggplot(just_plot_with_z %>% filter(stat == "goals"), aes(x = season, y = z_score, group = season)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = .1) +
  facet_wrap(.~ pos_broad, nrow = 1)



###############################
### z scores with career av ###
###############################

stat_av <- player_stats_long %>%
  filter(season >= 66) %>%
  group_by(pos, stat) %>%
  summarise(mean = mean(value),
            sd = sd(value))


# Merge with the zscores
z_scores_context <- z_scores %>%
  left_join(stat_av) %>%
  mutate(adj_value = mean + (z_score*sd),
         adj_value = ifelse(adj_value < 0, 0, adj_value))



# Career numbers 
career_numbers <- z_scores_context %>%
  group_by(id, stat) %>%
  summarise(total = sum(adj_value),
            name = last(name))



####################
### Career plots ###
####################


career_plot <- function(p_id, p_stat) {
  
  df <- player_stats_long %>%
    filter(stat == p_stat) %>%
    mutate(is_player = id == p_id)
  
  plot <- ggplot(df, aes(x = season, y = value, group = season)) +
    geom_boxplot(outlier.alpha = 0) +
    geom_jitter(data = df %>% filter(is_player == F),
                width = .1, 
                alpha = .2) +
    geom_point(data = df %>% filter(is_player == T),
               aes(fill = team),
               shape = 21,
               col = "black",
               size = 3,
               show.legend = F) +
    
    theme_bw(base_size = 16) +
    theme(panel.grid = element_blank()) +
    
    labs(x = "Season", y = p_stat, title = paste(last(df$name[df$id == p_id]), p_stat))
  return(plot)
  
  
}
