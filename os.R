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
