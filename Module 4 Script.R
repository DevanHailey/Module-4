# This is my code for Part 2 of Module 4
# The dataset I am analyzing is Tennis Grand Slams

library(tidyverse)

library(ggthemes)

# Getting the data
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# Data Dictionaries
grand_slam_timeline

grand_slams

player_dob

library(dplyr)

# Joining the Date of Birth dataset with the grandslam dataset
age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>% # needs to be datetime
  group_by(name, age, gender) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))

# test plot
age_slams_comb %>% 
  ggplot(aes(x = age/365, y = total_wins, group = name)) +
  geom_point() +
  geom_step() +
  facet_wrap(~gender) +
  theme_few(base_size = 12)

# the age was in days so I divided it by 365
# to get a more reasonable age in years

ggsave("Module_4_Plot_Tennis.pdf", width = 10, height = 8, units = "in")

