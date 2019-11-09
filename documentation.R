library(tidyverse)
library(lubridate)
source("api-analytics.R")

hostnames <- vroom::vroom("hostnames.csv")

# Documentation and articles -------------------------------------------------

ref_pages <- googleAnalyticsR::filter_clause_ga4(operator = "OR", list(
  googleAnalyticsR::dim_filter("pagePath", "PARTIAL", "/reference/"),
  googleAnalyticsR::dim_filter("pagePath", "PARTIAL", "/articles/")
))

topics_raw <- ga_get(
  dimensions = c("hostname", "pagePath"),
  dim_filters = ref_pages
)

topics <- topics_raw %>%
  mutate(site = str_match(hostname, "(.*)\\.(tidyverse|r-lib)\\.org")[, 2]) %>%
  filter(!is.na(site)) %>%
  select(site, pagePath, sessions, users) %>%
  arrange(desc(sessions))

topics

topics %>%
  filter(site == "dplyr", sessions > 500) %>%
  print(n = 20)

# Blog posts ----------------------------------------------------------------

filter_blog <- googleAnalyticsR::filter_clause_ga4(
  list(
    googleAnalyticsR::dim_filter("pagePath", "PARTIAL", "/articles/"),
    googleAnalyticsR::dim_filter("hostname", "EXACT", "www.tidyverse.org")
  ),
  operator = "AND"
)

blog_raw <- ga_get(
  dimensions = "pagePath",
  dim_filters = filter_blog
)

blog_raw %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

# r4ds --------------------------------------------------------------------

filter_r4ds <- googleAnalyticsR::filter_clause_ga4(list(
  googleAnalyticsR::dim_filter("hostname", "EXACT", "r4ds.had.co.nz")
))

r4ds_raw <- ga_get(
  dimensions = "pagePath",
  dim_filters = filter_r4ds
)

r4ds_raw %>%
  filter(users > 20) %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

r4ds_raw %>%
  mutate(pagePath = str_replace(pagePath, "\\?.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, "\\#.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, ".htm$", ".html")) %>%
  count(pagePath, wt = users, sort = TRUE)


# Explore by time ---------------------------------------------------------

r4ds_raw <- ga_get(
  dimensions = c("date", "pagePath"),
  dim_filters = filter_r4ds
)

r4ds <- r4ds_raw %>%
  mutate(pagePath = str_replace(pagePath, "\\?.*$", "")) %>%
  mutate(week = floor_date(date, "week", week_start = 1))

r4ds_weekly <- r4ds %>%
  filter(pagePath != "/") %>% # home page isn't that interesting
  group_by(week, pagePath) %>%
  summarise(sessions = sum(sessions)) %>%
  filter(sessions > 10)

r4ds_weekly %>%
  ggplot(aes(week, sessions, group = pagePath)) +
  geom_line() +
  geom_point() +
  scale_y_log10()
