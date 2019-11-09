library(tidyverse)
library(lubridate)
source("utils/analytics.R")

# Rough overview of all traffic, broken down by date and site

# Total -------------------------------------------------------------------

analytics()
analytics(from = today() - 365, to = today())

# Weekly users/sessions ---------------------------------------------------

weekly <- analytics_weekly()
weekly %>%
  head(-1) %>%
  pivot_longer(users:sessions, names_to = "var", values_to = "n") %>%
  ggplot(aes(week, n, colour = var)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line() +
  labs(
    title = "Weekly tidyverse website stats",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme(legend.position = "bottom")

# By site -----------------------------------------------------------------

by_site <- analytics("hostname")

by_site %>%
  filter(is_tidyverse(hostname)) %>%
  ggplot(aes(fct_reorder(hostname, users), users)) +
  geom_point() +
  scale_y_log10(
    breaks = scales::log_breaks(n = 6),
    labels = scales::comma, minor_breaks = 10
  ) +
  coord_flip() +
  labs(
    title = "Last month",
    x = NULL,
    y = "Total sessions (log scale)"
  )
