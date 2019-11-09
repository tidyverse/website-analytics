library(tidyverse)
source("api-analytics.R")

alphabetise <- function(x) {
  r_match <- "\\b(in )?r\\b"
  has_r <- str_detect(x, r_match)

  out <- x %>%
    str_replace_all(r_match, "") %>%
    str_squish() %>%
    str_split(" ") %>%
    map(str_sort) %>%
    map_chr(str_flatten, " ")

  out[has_r] <- paste0(out[has_r], " [r]")
  out
}

group_terms <- function(df) {
  df %>%
    mutate(query_a = alphabetise(query)) %>%
    group_by(query_a) %>%
    summarise(
      query = list(query),
      clicks = sum(clicks),
      position = weighted.mean(position, impressions),
      impressions = sum(impressions),
    ) %>%
    select(query_a, query, clicks, impressions, position) %>%
    mutate(ctr = clicks / impressions) %>%
    arrange(desc(clicks))
}

readxl <- sc_get_queries("readxl.tidyverse.org") %>%
  filter(clicks > 0) %>%
  group_terms()

readxl %>% ggplot(aes(ctr, wt = impressions)) + geom_histogram(binwidth = 0.02)

# Code searches tend to have high ctr?
readxl %>% filter(str_detect(query_a, "_"))

# See original queries
grouped %>%
  filter(query_a == "package readxl [r]") %>%
  pull(query) %>%
  .[[1]]

# Other sites -------------------------------------------------------------

readr <- sc_get_queries("readr.tidyverse.org") %>%
  filter(clicks > 0) %>%
  group_terms()

readr
readr %>% filter(str_detect(query_a, "_"))
