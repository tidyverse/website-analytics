library(tidyverse)
library(lubridate)
source("utils/analytics.R")

# These really need to be one reported per

# Documentation and articles -------------------------------------------------

filter_docs <- filter_or(
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/reference/"),
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/articles/")
)

topics_raw <- analytics(c("hostname", "pagePath"), dim_filters = filter_docs)

topics <- topics_raw %>%
  filter(is_tidyverse(hostname)) %>%
  mutate(package = package_name(hostname), hostname = NULL) %>%
  select(package, pagePath, sessions, users) %>%
  arrange(desc(sessions))

topics

topics %>%
  filter(site == "dplyr", sessions > 500) %>%
  print(n = 20)

# Blog posts ----------------------------------------------------------------

filter_blog <- filter_and(
  googleAnalyticsR::dim_filter("hostname", "EXACT", "www.tidyverse.org"),
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/articles/")
)

blog_raw <- analytics("pagePath", dim_filters = filter_blog)

blog_raw %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

# Not sure what we want here? Maybe top articles for last month?
# Top articles in last year?

# r4ds --------------------------------------------------------------------

filter_r4ds <- filter_or(
  googleAnalyticsR::dim_filter("hostname", "EXACT", "r4ds.had.co.nz")
)

r4ds_raw <- analytics("pagePath", dim_filters = filter_r4ds)

r4ds_raw %>%
  filter(users > 20) %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

# Trimming anchors and query strings doesn't change overall numbers that much
r4ds_raw %>%
  mutate(pagePath = str_replace(pagePath, "\\?.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, "\\#.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, ".htm$", ".html")) %>%
  count(pagePath, wt = users, sort = TRUE)

# Look at patterns over time:
# there's not much going on (unsurprisingly)
r4ds_raw <- analytics_weekly("pagePath", dim_filters = filter_r4ds)
r4ds_raw %>%
  mutate(pagePath = fct_lump(pagePath, n = 10, w = sessions)) %>%
  group_by(week, pagePath) %>%
  summarise(sessions = sum(sessions)) %>%
  ggplot(aes(week, sessions, colour = fct_reorder2(pagePath, week, sessions))) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(colour = "path")
