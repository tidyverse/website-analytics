library(tidyverse)
library(lubridate)
source("utils/analytics.R")

# Diagnostics to check if we have accidentally broken google analytics
# on any existing sites

by_site <- analytics_weekly(
  from = today() - weeks(12),
  dimensions = "hostname")
)

# sanity check is_tidyverse() function
by_site %>%
  count(hostname, wt = sessions, sort = TRUE) %>%
  filter(!is_tidyverse(hostname))

by_site %>%
  filter(is_tidyverse(hostname)) %>%
  count(week) %>%
  ggplot(aes(week, n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "GA usage",
    x = NULL,
    y = "Sites"
  )

missing_records <- by_site %>%
  filter(is_tidyverse(hostname)) %>%
  complete(week, hostname) %>%
  group_by(hostname) %>%
  summarise(n_missing = sum(is.na(users))) %>%
  filter(n_missing > 0)

# What sites have changed their inclusion status?
by_site %>%
  semi_join(missing_records) %>%
  ggplot(aes(week, fct_rev(fct_infreq(hostname)))) +
  geom_tile() +
  scale_x_date(NULL, expand = c(0, 0), minor_breaks = "week") +
  scale_y_discrete("Site", expand = c(0, 0)) +
  labs(
    title = "Usage by site",
    x = NULL,
    y = NULL
  )

daily <- analytics(c("hostname", "date"))
daily %>%
  filter(is_tidyverse(hostname)) %>%
  semi_join(missing_records, by = "hostname") %>%
  complete(hostname, date, fill = list(users = 0, sessions = 0)) %>%
  ggplot(aes(date, users)) +
  geom_line() +
  facet_wrap(~ hostname, scales = "free_y")
