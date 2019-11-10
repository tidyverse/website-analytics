library(tidyverse)
source("utils/search-console.R")

readxl <- sc_get_queries("readxl.tidyverse.org") %>%
  filter(clicks > 0) %>%
  group_terms()
readxl

readxl %>%
  ggplot(aes(ctr, wt = impressions)) +
  geom_histogram(binwidth = 0.02)

# Code searches tend to have high ctr?
readxl %>% filter(str_detect(query_a, "_"))

# See original queries
readxl %>%
  filter(query_a == "package readxl [r]") %>%
  pull(query) %>%
  .[[1]]

# Other sites -------------------------------------------------------------

readr <- sc_get_queries("readr.tidyverse.org") %>%
  filter(clicks > 0) %>%
  group_terms()

readr
readr %>% filter(str_detect(query_a, "_"))
