library(tidyverse)
library(lubridate)
source("utils/analytics.R")

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
