tidytuesdayR::tt_load("2022-01-11")
##load data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

#load packages
pacman::p_load(ggplot2, tidyverse, scales)

#To make a new column with the variable colony_netto 
Colony_netto <- colony %>% 
  mutate(netto = colony_added - colony_lost)
print(Colony_netto)

#To make a dataset with only dat for the entire US 
US_total <- Colony_netto %>%  
  filter(state == "United States")

US_total

#We want to plot year as a continous variable with the month data 
US_total$months_num <- recode(US_total$months, "January-March" = 0, 	
                              "April-June" = 0.25,
                              "July-September" = 0.5,
                              "October-December" = 0.75)

#new column with the year and month merged 
US_total_new_month <- US_total %>% 
  mutate(year_month = (year + months_num))

#Plot of the Bee balance in the US during the years 
US_total_new_month %>% 
  ggplot(aes(year_month, netto)) +
  geom_line() +
  theme_minimal(base_size = 16) +
  labs(x = "Year",
       y =  "Bee Balance") +
  ggtitle("Bee Balance in The United States") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  scale_y_continuous(breaks = c(-600000, -400000, -200000, 0,200000, 400000, 600000), 
                     limits = c(-400000, 500000), 
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(2015:2021), minor_breaks = seq(2015, 2021, 0.25))

#Plot with lost bees accumulation 
US_total_new_month %>% 
  ggplot(aes(year_month, cumsum(replace_na(colony_lost, 0))/1e6)) +
  geom_line(color = "red") +
  theme_minimal(base_size = 16) +
  labs(x = "Year",
       y =  "Colonies Lost (Millions)") +
  ggtitle("Lost Bee Colonies in The United States") +
  scale_y_continuous(breaks = c(0:10), 
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(2015:2021), minor_breaks = seq(2015, 2021, 0.25))

#Plot with added bees accumulation
US_total_new_month %>% 
  ggplot(aes(year_month, cumsum(replace_na(colony_added, 0))/1e6)) +
  geom_line(color = "green") +
  theme_minimal(base_size = 16) +
  labs(x = "Year",
       y =  "Colonies lost (millions)") +
  ggtitle("Lost Bee Colonies in The United States") +
  scale_y_continuous(breaks = c(0:10), 
                     labels = label_comma()) +
  scale_x_continuous(breaks = c(2015:2021), minor_breaks = seq(2015, 2021, 0.25))

#% colonies lost
US_total_new_month %>% 
  ggplot(aes(year_month, colony_lost_pct, fill = months)) +
  geom_col(color = "yellow") +
  theme_minimal(base_size = 16) +
  scale_x_continuous(breaks = c(2015:2021)) +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
  labs(x = "Year",
       y =  "Colonies lost (%)",
       fill = "Month") +
  ggtitle("Lost Bee Colonies in The United States")


#Old code, based on year 
colony_balance <- colony %>% 
  group_by(year, state) %>% 
  summarise(year_sum = sum(colony_added - colony_lost, na.rm = TRUE))

colony_balance %>%  
  filter(state != "United States") %>% 
  ggplot(aes(year, year_sum)) +
  geom_line() +
  facet_wrap(~state) +
  theme_minimal() 


US <- colony_balance %>%  
  filter(state == "United States")

US %>% 
  ggplot(aes(year, year_sum)) +
  geom_line() +
  theme_minimal(base_size = 16) +
  scale_y_continuous(breaks = c(-600000, -400000, -200000, 0,200000, 400000), 
                     limits = c(-600000, 400000), 
                     labels = label_comma()) +
  labs(x = "Year",
       y =  "Bee Balance") +
  ggtitle("Bee Balance in The United States") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")


