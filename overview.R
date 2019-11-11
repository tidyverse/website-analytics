library(tidyverse)
library(lubridate)
source("utils/analytics.R")

# Rough overview of all traffic, broken down by date and site

# Total -------------------------------------------------------------------

analytics()
analytics(from = today() - 365, to = today())

# Weekly users/sessions ---------------------------------------------------

weekly <- analytics_weekly()
weekly %>%
  head(-1) %>%
  pivot_longer(users:sessions, names_to = "var", values_to = "n") %>%
  ggplot(aes(week, n, colour = var)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line() +
  labs(
    title = "Weekly tidyverse website stats",
    x = NULL,
    y = NULL,
    colour = NULL
  ) +
  theme(legend.position = "bottom")

# Dangerous to read too much into this plot as it's taken some time to
# get all major sites using google analytics.
# * tidytemplate accientally conditioned out analytics on Sep 3, and
#   wasn't fully restored until Nov 10.

# By site -----------------------------------------------------------------

by_site <- analytics("hostname")
by_site %>%
  filter(is_tidyverse(hostname)) %>%
  ggplot(aes(fct_reorder(hostname, users), users)) +
  geom_point() +
  scale_y_log10(
    breaks = scales::log_breaks(n = 6),
    labels = scales::comma, minor_breaks = 10
  ) +
  coord_flip() +
  labs(
    title = "Last month",
    x = NULL,
    y = "Total sessions (log scale)"
  )

site_weekly <- analytics_weekly("hostname")

# Weekly sessions for top sites
site_weekly %>%
  filter(is_tidyverse(hostname)) %>%
  lump_var(hostname) %>%
  ggplot(aes(week, sessions, colour = hostname)) +
  geom_line(size = 1) +
  labs(title = "Weekly sessions", x = NULL, y = "Sessions", colour = NULL) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "right", legend.justification = "top")

site_sessions <- function(hostname) {
  site_weekly %>%
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
    scale_colour_brewer(palette = "Set1") +
    theme(legend.position = "bottom")
}
site_sessions(c("dplyr.tidyverse.org", "r4ds.had.co.nz", "ggplot2.tidyverse.org"))
site_sessions(c("readxl.tidyverse.org", "readr.tidyverse.org", "haven.tidyverse.org", "vroom.r-lib.org"))
site_sessions(c("tidyr.tidyverse.org", "stringr.tidyverse.org", "purrr.tidyverse.org"))
site_sessions(c("lubridate.tidyverse.org", "forcats.tidyverse.org", "tibble.tidyverse.org"))
site_sessions(c("httr.r-lib.org", "rvest.tidyverse.org", "xml2.r-lib.org"))
site_sessions(c("magrittr.tidyverse.org", "glue.tidyverse.org", "reprex.tidyverse.org"))
site_sessions(c("usethis.r-lib.org", "roxygen2.r-lib.org", "testthat.r-lib.org", "devtools.r-lib.org", "pkgdown.r-lib.org"))
