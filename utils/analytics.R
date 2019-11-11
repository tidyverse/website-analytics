scopes <- c(
  "https://www.googleapis.com/auth/analytics",
  "https://www.googleapis.com/auth/webmasters"
)
googleAuthR::gar_auth_configure(path = "oauth-key.json")
googleAuthR::gar_auth(email = TRUE, scopes = scopes)


is_tidyverse <- function(x) {
  str_detect(x, "\\.(tidyverse\\.org|r-lib\\.org|r-dbi\\.org|had.co.nz)$")
}

package_name <- function(x) {
  str_match(x, "(.*)\\.(tidyverse|r-lib|r-dbi)\\.org$")[, 2]
}

filter_or <- function(...) {
  googleAnalyticsR::filter_clause_ga4(operator = "OR", list(...))
}
filter_and <- function(...) {
  googleAnalyticsR::filter_clause_ga4(operator = "AND", list(...))
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
  # google analytics week starts on a Sunday
  sunday <- function(x) x - wday(x) + 1

  to <- sunday(to) - 1L # so ends on a Saturday
  if (is.null(from)) {
    from <- to - weeks(52)
  } else {
    from <- sunday(from)
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


