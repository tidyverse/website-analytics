library(tidyverse)
library(lubridate)
source("utils/analytics.R")

filter_r4ds <- filter_or(
  googleAnalyticsR::dim_filter("hostname", "EXACT", "r4ds.had.co.nz")
)

analytics(dim_filters = filter_r4ds)

by_chapter <- analytics("pagePath", dim_filters = filter_r4ds)
by_chapter %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

# Trimming anchors and query strings doesn't change overall numbers that much
collapsed <- by_chapter %>%
  mutate(pagePath = str_replace(pagePath, "\\?.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, "\\#.*$", "")) %>%
  mutate(pagePath = str_replace(pagePath, ".html?$", "")) %>%
  mutate(pagePath = str_replace(pagePath, "/index$", "/")) %>%
  count(pagePath, wt = users, sort = TRUE)

# Look at patterns over time:
# there's not much going on (unsurprisingly)
by_chapter_week <- analytics_weekly("pagePath", dim_filters = filter_r4ds)
by_chapter_week %>%
  lump_var(pagePath, n = 15) %>%
  ggplot(aes(week, sessions)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~ pagePath)
