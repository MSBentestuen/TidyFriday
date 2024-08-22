library(tidyverse)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# Relationship between colonies lost and colonies added by state during 2015-2021.
colony_balance <- colony %>% 
  group_by(year, state) %>% 
  summarise(year_sum = sum(colony_added - colony_lost, na.rm = TRUE))

colony_balance %>% 
  filter(state != "United States") %>% 
  ggplot(aes(year, year_sum)) + 
  geom_line() + 
  facet_wrap(~ state) + 
  theme_minimal()


# Different plot using maps
