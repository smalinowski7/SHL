library(tidyverse)
library(here)
library(gganimate)
library(gifski)

source("scraper_functions.R")

contention_data <- read_csv("Projects/Contention cycle/contention_cycle.csv")

seasons_included <- c(85:89)
season_for_meta <- 88

colnames(contention_data) <- c("abbreviation", paste0(rep(seasons_included, each = 2), c("_now", "_future")))

meta <- index_meta(season_for_meta, league = 0)

colors <- ifelse(meta$abbreviation == "CIN", meta$colors_secondary, meta$colors_primary)
names(colors) <- meta$abbreviation


rect_data <- data.frame(
  x_start = c(0,2.5,5,7.5,0,2.5,5,7.5,0,2.5,5,7.5,0,2.5,5,7.5),
  x_end = c(2.5,5,7.5,10,2.5,5,7.5,10,2.5,5,7.5,10,2.5,5,7.5,10),
  y_start = c(0,0,0,0,2.5,2.5,2.5,2.5,5,5,5,5,7.5,7.5,7.5,7.5),
  y_end = c(2.5,2.5,2.5,2.5,5,5,5,5,7.5,7.5,7.5,7.5,10,10,10,10)
)

plot_data <- contention_data %>%
  mutate(abbreviation = ifelse(abbreviation == "NOLA", "NOL", abbreviation)) %>%
  pivot_longer(cols = -abbreviation,
               names_to = "season",
               values_to = "rating") %>%
  separate(season, into = c("season", "time")) %>%
  
  pivot_wider(names_from = time, 
              values_from = rating) 


static_plot <- ggplot(rect_data) +
  
  geom_rect(aes(xmin = x_start,
                xmax = x_end,
                ymin = y_start,
                ymax = y_end),
            fill = c("#D47E6F",
                     "#E0B7AB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#E0B7AB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#B7C6D7",
                     "#EFEFEB",
                     "#EFEFEB",
                     "#B7C6D7",
                     "#7C9EC4"
            ),
            col = "#999999",
            alpha = .75,
            linetype = "dotted") +
  
  geom_rect(xmin = 3.75,
            xmax = 6.25,
            ymin = 3.75,
            ymax = 6.25,
            fill = "#DBDAD8") +

  
  theme_bw(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = .5)) +
  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  
  annotate("text", x = 9.25, y = 9.5, label = "Window\nOpen", col = "#2854C5") +
  annotate("text", x = 9.5, y = 5, label = "Window Closing", col = "#2854C5", angle = 270) +
  annotate("text", x = 5, y = 9.5, label = "Window Opening", col = "#2854C5") +
  annotate("text", x = .75, y = 9.5, label = "Win\nLater", col = "#999999") +
  annotate("text", x = .5, y = 5, label = "Rebuilding", col = "#BB281A", angle = 90) +
  annotate("text", x = .75, y = .5, label = "Window\nClosed", col = "#BB281A") +
  annotate("text", x = 5, y = .5, label = "Time to Rebuild", col = "#BB281A") +
  annotate("text", x = 9.25, y = .5, label = "Win\nNow", col = "#999999") +
  annotate("text", x = 5, y = 5, label = "No\nMan's\nLand", col = "black") +
  
  coord_equal() +
  geom_label(data = plot_data,
             aes(x = now, y= future, label = abbreviation, col = abbreviation),
             show.legend = F) +
  scale_color_manual(values = colors) +
  labs(x = "Present Rating", y = "Future Rating")


anim = static_plot + transition_states(season, transition_length = 4, state_length = 1) +
  #view_follow(fixed_x = TRUE)  +
  labs(title = 'The SHL Contention Cycle - S{closest_state}')



animate(anim, 200, fps = 30,  duration = length(seasons_included)*.75, width = 750, height = 500,
        renderer = gifski_renderer("contentioncyclefast.gif"))
