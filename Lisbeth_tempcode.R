
#install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
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
  geom_line(size= 1) +
  facet_wrap(~state) +
  theme_minimal() +
  scale_color_gradient(low = "black", high = "yellow",
                      name = "Bee balance", label = scales::comma) + 
  labs(x = "Year",
       y = "Yearly balance") +
  scale_y_continuous(label = scales::comma)

#install.packages("usmap")
library(usmap)

plot_usmap()



#Overall map
plot_usmap(data = data, values = "colony_netto_sum", color = "black",
           exclude = c("Alaska", "Nevada", "New Hampshire", "Delaware", "Rhode Island", "District of Columbia")) +
  scale_fill_continuous(low = "black", high = "yellow",
                        name = "Bee balance", label = scales::comma)  + 
  facet_wrap(~year, nrow = 2) +
  theme(axis.text.y = element_blank()) +
  theme_void()

#One year
wide <- data %>%
  select(state, year, colony_netto_sum) %>%
  tidyr::pivot_wider(,
                     names_from = year,
                     values_from = colony_netto_sum)

plot_usmap(data = wide, values = "2021", color = "black") + 
  scale_fill_continuous(low = "black", high = "yellow",
                        name = "Netto development (2021)", label = scales::comma)  + 
  theme(legend.position = "right")


total <- data %>%
  filter(year == 2015 | year == 2021) %>%
  select(state, year, colony_netto_sum) 

wide <- tidyr::pivot_wider(total,
                     names_from = year,
                     values_from = colony_netto_sum)

new <- wide %>%
  mutate(difference = 2021 - 2015)

unique(data$state)

colony$months_rc <- recode(colony$months, "January-March" = 0)


















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