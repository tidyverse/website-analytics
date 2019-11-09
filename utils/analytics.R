scopes <- c(
  "https://www.googleapis.com/auth/analytics",
  "https://www.googleapis.com/auth/webmasters"
)
googleAuthR::gar_auth_configure(path = "oauth-key.json")
googleAuthR::gar_auth(email = TRUE, scopes = scopes)


is_tidyverse <- function(x) {
  str_detect(x, "(\\.tidyverse\\.org|r-lib\\.org|had\\.co\\.nz|r-dbi\\.org)$")
}

# google analytics --------------------------------------------------------

analytics <- function(
                   dimensions = c(), ...,
                   metrics = c("users", "sessions"),
                   from = "31daysAgo",
                   to = "yesterday") {
  out <- googleAnalyticsR::google_analytics(
    dimensions = dimensions,
    viewId = 170811407,
    metrics = metrics,
    ...,
    date_range = c(from, to),
    max = -1
  )
  tibble::as_tibble(out)
}

# Get last year of data, starting and ending on a monday.
analytics_weekly <- function(dimensions = c(), ..., from = NULL, to = today() - 1L) {
  monday <- function(x) x - (wday(x) - 1)

  to <- monday(to)
  if (is.null(from)) {
    from <- to - weeks(52)
  } else {
    from <- monday(from)
  }

  dimensions <- c(dimensions, "year", "week")
  df <- analytics(
    ...,
    from = from,
    to = to,
    dimensions = dimensions
  )
  df %>%
    mutate(
      week = make_date(as.numeric(year)) + weeks(as.numeric(week) - 1) - days(1),
      year = NULL
    )
}


