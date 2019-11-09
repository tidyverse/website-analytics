library(tidyverse)
library(lubridate)
source("api-analytics.R")
library(cranlogs)

date_end <- today() - days(3)
date_start <- date_end - days(30)

# Google analytics --------------------------------------------------------

daily <- ga_get(
  from = date_start,
  to = date_end,
  metrics = c("hits", "sessions", "users"),
  dimensions = c("date", "hostname"),
)

sites <- daily %>%
  extract(hostname, "package", "^(.*)\\.tidyverse\\.org$", remove = FALSE) %>%
  distinct(hostname, package) %>%
  filter(!is.na(package))

package_hits <- daily %>%
  inner_join(sites, by = "hostname") %>%
  select(date, package, site_hits = hits, site_users = users, site_sessions = sessions)

# Google searches ---------------------------------------------------------

site_searches <- function(url) {
  filter <- paste0("page~~", url)
  sc_get(dimension = "date", filter = filter, from = date_start, to = date_end)
}

searches_raw <- sites$hostname %>% set_names() %>% map_dfr(site_searches, .id = "hostname")

package_searches <- searches_raw %>%
  inner_join(sites, by = "hostname") %>%
  select(date, package, search_clicks = clicks, search_views = impressions)

# Package downloads -------------------------------------------------------

downloads <- cran_downloads(sites$package, from = date_start, to = date_end)

package_downloads <- downloads %>%
  as_tibble() %>%
  select(date, package, downloads = count)

# All together ------------------------------------------------------------

anti_join(package_hits, package_downloads)
anti_join(package_downloads, package_hits)

package_info <- package_hits %>%
  left_join(package_searches, by = c("date", "package")) %>%
  left_join(package_downloads, by = c("date", "package"))

package_sum <- package_info %>%
  group_by(package) %>%
  summarise_if(is.numeric, sum) %>%
  filter(downloads != 0, package != "blob") %>%
  mutate(
    prop_from_search = search_clicks / site_hits,
    users_per_download = site_users / downloads
    )

View(package_sum)

package_sum %>%
  filter(package != "googledrive") %>%
  ggplot(aes(downloads, site_users)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()



package_sum %>% filter(downloads < 20000) %>% pull(package)

package_sum %>%
  arrange(desc(prop_from_search)) %>%
  select(package, search_clicks, site_hits, prop_from_search) %>%
  print(n = Inf)

sc_get_queries("rvest.tidyverse.org")
sc_get_queries("glue.tidyverse.org")
