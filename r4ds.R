library(tidyverse)
library(lubridate)
library(googleAnalyticsR)

ga_auth()
view_id <- 170811407

# Trend over time ---------------------------------------------------------

trend <- as_tibble(google_analytics(
  view_id,
  date_range = c("2019-01-08", "yesterday"),
  metrics = c("hits", "sessions", "users"),
  dimensions = "date",
  dim_filters = filter_clause_ga4(list(dim_filter("hostname", "EXACT", "r4ds.had.co.nz"))),
  max = -1
))

trend %>%
  group_by(week = floor_date(date, "week")) %>%
  summarise(sessions = sum(sessions), n = n()) %>%
  filter(n == 7) %>%
  ggplot(aes(week, sessions)) +
  geom_line()

# Most popular pages ------------------------------------------------------

pages <- as_tibble(google_analytics(
  view_id,
  date_range = c("8daysAgo", "yesterday"),
  metrics = c("hits", "sessions", "users"),
  dimensions = "pagePath",
  dim_filters = filter_clause_ga4(list(dim_filter("hostname", "EXACT", "r4ds.had.co.nz"))),
  max = -1
))
pages

pages %>%
  mutate(pagePath = str_replace(pagePath, "\\?.+$", "")) %>%
  count(pagePath, wt = hits, sort = TRUE) %>%
  print(n = 20)

