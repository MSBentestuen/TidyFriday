# title: Temporary code 
# description: testing different codes for TidyFriday project
# author: MSB
# last edit: 2024-08-23

# Libraries ---------------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse, sf, ggrepel, gganimate, gifski, ggpubr, scales, zoo)

# Load data: Bee colonies lost --------------------------------------------
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

# Compute "bee-balance" per state per year
bee_balance <- colony %>% 
  filter(state != "United States" & state != "Other States") %>% 
  group_by(year, state) %>% 
  summarise(year_balance = sum(colony_added - colony_lost, na.rm = TRUE))

# Create shape file (sf) object 
usa_sf <- read_sf("usa.json")
usa_sf <- usa_sf %>% 
  rename(state = NAME)

# Merge with bee data: 
usa_sf_merged <- usa_sf %>%
  left_join(bee_balance, by = "state") %>% 
  filter(state != "Alaska")   # no data for Alaska

# Create State Names
coordinates_state_names <- data.frame(state = usa_sf_merged$state, 
                                      coordinates = usa_sf_merged$geometry) %>% 
  distinct() %>% 
  filter(state %in% c("Texas", "California", "Florida", "North Dakota"))

# Choropleth map:
map <- ggplot(usa_sf_merged) +
  geom_sf(aes(fill = year_balance), linewidth = 0, alpha = 0.9) +
  ggrepel::geom_label_repel(
    data = coordinates_state_names,
    aes(label = state, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0, 
    force = 300,
    seed = 8, 
    size = 7
  ) +
  theme_void(
    base_size = 16
  ) +
  scale_fill_gradient2() +
  labs(title = "Bee Balance in {round(frame_time)}", 
       fill = "Balance") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 32, hjust = 0.01, color = "#4e4d47",
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
    )
  ) + 
  transition_time(year)

map_anim1 <- animate(map, fps = 10, 
                     height = 974, width = 1275, renderer = gifski_renderer())

anim_save("bee_balance_years.gif", map_anim1)
