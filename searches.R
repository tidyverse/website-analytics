library(lubridate)
library(tidyverse)
source("utils/search-console.R")

sc_get("query")
sc_get("date")
sc_get("page")

# How many people are finding tidyverse sites from google over the last year?
over_time <- sc_get("date", from = last_year())
over_time %>% summarise(
  impressions = sum(impressions),
  clicks = sum(clicks)
) %>%
  mutate(
    ctr = clicks / impressions,
  )

over_time %>%
  mutate(
    weekend = ifelse(wday(date, label = TRUE) %in% c("Sat", "Sun"), "weekend", "weekday"),
  ) %>%
  filter(position < 20) %>%
  ggplot(aes(date, position)) +
  geom_hline(yintercept = 12, size = 2, colour = "white") +
  geom_smooth(se = FALSE, size = 2, colour = "grey70") +
  geom_point() +
  facet_wrap(~ weekend, ncol = 1) +
  scale_y_reverse() +
  labs(
    title = "Search ranking across all tidyverse sites",
    y = "Position in search query",
    x = NULL
  )


over_time %>%
  group_by(date = floor_date(date, "week")) %>%
  summarise(clicks = sum(clicks), days = n()) %>%
  filter(days == 7) %>%
  ggplot(aes(date, clicks)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = scales::comma)

# How are people finding us? ----------------------------------------------

search_queries("stringr.tidyverse.org")
search_queries("ggplot2.tidyverse.org")
search_queries("stringr.tidyverse.org")
search_queries("dplyr.tidyverse.org")

search_queries("www.tidyverse.org/articles/")
search_queries("www.tidyverse.org/articles/2017/12/workflow-vs-script/")

search_queries("ggplot2.tidyverse.org/reference/geom_bar.html")
search_pages("geom_bar")
search_pages("ggplot2 bar chart")
geom_bar <- search_queries("ggplot2.tidyverse.org/reference/geom_bar.html")

geom_bar %>%
  mutate(geom_bar = str_detect(query, "geom_bar|geom bar")) %>%
  count(geom_bar, wt = clicks) %>%
  mutate(prop = n / sum(n))

geom_smooth <- search_queries("ggplot2.tidyverse.org/reference/geom_smooth.html")

geom_smooth %>%
  mutate(geom_bar = str_detect(query, "geom_smooth|geom smooth")) %>%
  count(geom_bar, wt = clicks) %>%
  mutate(prop = n / sum(n))
