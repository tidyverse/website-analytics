library(tidyverse)

by_site <- analytics_weekly("hostname")

# Top
by_site %>%
  filter(is_tidyverse(hostname)) %>%
  group_by(week, hostname = fct_lump(hostname, n = 7, w = sessions)) %>%
  summarise(n = sum(sessions)) %>%
  ungroup() %>%
  mutate(hostname = fct_reorder2(hostname, week, n)) %>%
  ggplot(aes(week, n, colour = hostname)) +
  geom_line(size = 1) +
  labs(title = "Weekly sessions", x = NULL, y = "Sessions", colour = NULL) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

site_sessions <- function(hostname) {
  by_site %>%
    filter(.data$hostname %in% c(.env$hostname)) %>%
    ggplot(aes(week, sessions, colour = fct_reorder2(hostname, week, sessions))) +
    geom_line(size = 1) +
    scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Sessions per week",
      x = NULL,
      y = "Sessions",
      colour = NULL
    ) +
    theme(legend.position = "bottom")
}
site_sessions(c("dplyr.tidyverse.org", "r4ds.had.co.nz", "ggplot2.tidyverse.org"))

site_sessions(c("readxl.tidyverse.org", "readr.tidyverse.org", "haven.tidyverse.org", "vroom.r-lib.org"))

site_sessions(c("tidyr.tidyverse.org", "stringr.tidyverse.org"))
site_sessions(c("lubridate.tidyverse.org", "forcats.tidyverse.org", "tibble.tidyverse.org"))

site_sessions(c("httr.r-lib.org", "rvest.tidyverse.org", "xml2.r-lib.org"))

site_sessions(c("magrittr.tidyverse.org", "glue.tidyverse.org", "purrr.tidyverse.org"))

site_sessions(c("usethis.r-lib.org", "roxygen2.r-lib.org", "testthat.r-lib.org", "devtools.r-lib.org", "pkgdown.r-lib.org"))

site_sessions("reprex.tidyverse.org")
