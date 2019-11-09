
# suppress broken check
assignInNamespace("check.Url", function(url, ...) url, "searchConsoleR")

# "today" in search console time is 3 days ago
today <- function() lubridate::today() - lubridate::days(3)
last_week <- function() today() - lubridate::weeks(1)
last_month <- function() today() - lubridate::period(months = 1)
last_quarter <- function() today() - lubridate::months(3)
last_year <- function() today() - lubridate::years(1)

sc_get <- function(dimension, filter = NULL, from = last_month(), to = today()) {
  # dimension <- rlang::arg_match(dimension, c("query", "date", "country", "device", "page"))

  out <- suppressMessages(searchConsoleR::search_analytics(
    "sc-domain:tidyverse.org",
    dimensions = dimension,
    dimensionFilterExp = filter,
    startDate = from,
    endDate = to
  ))

  tibble::as_tibble(out)
}

sc_get_queries <- function(url, from = last_month(), to = today()) {
  filter <- paste0("page~~", url)
  sc_get(dimension = "query", filter = filter, from = from, to = to)
}

sc_get_pages <- function(query, from = last_month(), to = today()) {
  filter <- paste0("query==", query)
  sc_get(dimension = "page", filter = filter, from = from, to = to)
}
