
#install.packages("tidytuesdayR")
library(tidytuesdayR)
tidytuesdayR::tt_available()

data <- tidytuesdayR::tt_load('2022-01-11')
colony <- data$colony
stressor <- data$stressor
print(colony)

pacman::p_load(tidyverse, dplyr)

#https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-11

#Summarize by year
data <- colony %>%
  mutate(colony_netto = colony_added - colony_lost) %>%
  group_by(year, state) %>%
  summarize(colony_n_mean = mean(colony_n, na.rm = TRUE),
            colony_added_sum = sum(colony_added, na.rm = TRUE),
            colony_lost_sum = sum(colony_lost, na.rm = TRUE),
            colony_netto_sum = sum(colony_netto, na.rm = TRUE)) %>%
  ungroup %>%
  filter(state != "United States")

#Compute total netto development
data <- data %>%
  group_by(state) %>%
  mutate(total_net = sum(colony_netto_sum)) %>%
  ungroup()

#Plot for each state
ggplot(data = data, aes(x = year, y = colony_netto_sum, color = total_net)) + 
  geom_line() +
  facet_wrap(~state) +
  theme_bw()

#install.packages("usmap")
library(usmap)

plot_usmap()

wide <- data %>%
  select(state, year, colony_netto_sum) %>%
  tidyr::pivot_wider(,
                     names_from = year,
                     values_from = colony_netto_sum)

plot_usmap(data = test, values = "pop_2022", color = "red") + 
  scale_fill_continuous(name = "Population (2022)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = wide, values = "2015", color = "black") 



total <- data %>%
  filter(year == 2015 | year == 2021) %>%
  select(state, year, colony_netto_sum) 

wide <- tidyr::pivot_wider(total,
                     names_from = year,
                     values_from = colony_netto_sum)

new <- wide %>%
  mutate(difference = 2021 - 2015)




















#Old code
wide <- tidyr::pivot_wider(total,
                           names_from = year,
                           values_from = colony_netto_sum,
                           names_glue = "{colony_netto_sum}_{year}")
total$year <- as.factor(total$year)
wide <- reshape(total, idvar = "state", timevar = "year", direction = "wide")

df3 <- data.frame(school = rep(1:3, each = 4), class = rep(9:10, 6),
                  time = rep(c(1,1,2,2), 3), score = rnorm(12))
wide <- reshape(df3, idvar = c("school","class"), direction = "wide")
wide

ggplot(data = data, aes(x = year, y = colony_n_mean, color = state)) + 
  geom_line() +
  scale_y_log10() + 
  facet_wrap(~state) +
  theme_bw()



























mutate(year2015 = ifelse(year == 2015, 1, 0),
       year2021 = ifelse(year == 2021, 1, 0)) %>%