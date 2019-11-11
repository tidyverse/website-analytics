library(tidyverse)
library(lubridate)
source("utils/analytics.R")
source("utils/search-console.R")

# Need to figure out deal with name change
filter_blog <- filter_and(
  googleAnalyticsR::dim_filter("hostname", "EXACT", "www.tidyverse.org"),
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/articles/")
)

analytics(dim_filters = filter_blog)
search_queries("www.tidyverse.org/articles", duration = "1 year")

by_page <- analytics("pagePath", dim_filters = filter_blog)
by_page %>%
  arrange(desc(sessions)) %>%
  print(n = 20)

# Blog shows fundamentally different temporal pattern:
by_week <- analytics_weekly(dim_filters = filter_blog)
by_week %>%
  ggplot(aes(week, sessions)) +
  geom_line()

# Because the vast majority of blog posts decay expoentially after release
by_week_page <- analytics_weekly("pagePath", dim_filters = filter_blog)

posts <- by_week_page %>%
  filter(!str_detect(pagePath, fixed("?"))) %>%
  separate(pagePath, c(NA, NA, "year", "month", "title"), "/", remove = FALSE) %>%
  filter(title != "") %>%
  mutate(label = str_glue("{title}\n{year}-{month}"))

top_posts <- posts %>%
  group_by(pagePath) %>%
  filter(n() > 4) %>%
  summarise(
    sum = sum(sessions),
    mean = mean(sessions),
    max = max(sessions),
    sd = sd(sessions),
    n = n(),
    deriv = max(abs(diff(sessions) / diff(as.numeric(week))))
  )

# Max
posts %>%
  semi_join(top_posts %>% arrange(desc(max)) %>% head(12), by = "pagePath") %>%
  mutate(label = fct_reorder(label, sessions, max)) %>%
  ggplot(aes(week, sessions, group = pagePath)) +
  geom_line() +
  facet_wrap(~ label)

# Sum
posts %>%
  semi_join(top_posts %>% arrange(desc(sum)) %>% head(12), by = "pagePath") %>%
  mutate(label = fct_reorder(label, sessions, sum)) %>%
  ggplot(aes(week, sessions, group = pagePath)) +
  geom_line() +
  facet_wrap(~ label)

# Not sure what we want here? Maybe top articles for last month?
# Top articles in last year?

search_queries("www.tidyverse.org/articles/2017/12/workflow-vs-script/", duration = "1 year")
# call `rlang::last_error()` to see a backtrace
search_queries("www.tidyverse.org/articles/2018/10/rlang-0-3-0/", duration = "1 year")
# scales.r-lib.org is not doing well on search
search_queries("www.tidyverse.org/articles/2018/08/scales-1-0-0/", duration = "1 year")
