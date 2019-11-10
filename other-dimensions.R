library(tidyverse)
library(lubridate)
source("utils/analytics.R")

os <- analytics_weekly("operatingSystem")
os %>%
  group_by(week, os = fct_lump(operatingSystem, 5, w = sessions)) %>%
  summarise(users = sum(users), sessions = sum(sessions)) %>%
  mutate(prop = users / sum(users)) %>%
  ggplot(aes(week, prop, colour = fct_reorder2(os, week, prop))) +
  geom_line() +
  labs(colour = "OS")

country <- analytics_weekly("country")
country %>%
  group_by(week, country = fct_lump(country, 8, w = sessions)) %>%
  summarise(users = sum(users), sessions = sum(sessions)) %>%
  mutate(prop = users / sum(users)) %>%
  ggplot(aes(week, sessions, colour = fct_reorder2(country, week, sessions))) +
  geom_line() +
  labs(colour = NULL) +
  scale_y_log10(labels = scales::comma) +
  scale_colour_brewer(palette = "Set1")

language <- analytics_weekly("language")
language %>%
  group_by(week, language = fct_lump(language, 8, w = sessions)) %>%
  summarise(users = sum(users), sessions = sum(sessions)) %>%
  mutate(prop = users / sum(users)) %>%
  ggplot(aes(week, sessions, colour = fct_reorder2(language, week, sessions))) +
  geom_line() +
  labs(colour = NULL) +
  scale_y_log10(labels = scales::comma) +
  scale_colour_brewer(palette = "Set1")


gender <- analytics_weekly("userGender")
