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
