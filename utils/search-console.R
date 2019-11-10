library(searchConsoleR)
searchConsoleR::scr_auth(email = TRUE)

# suppress broken check
assignInNamespace("check.Url", function(url, ...) url, "searchConsoleR")

# "today" in search console time is 3 days ago

sc_get <- function(dimension, filter = NULL, from = last_month(), to = today()) {
  # dimension <- rlang::arg_match(dimension, c("query", "date", "country", "device", "page"))
  today <- function() lubridate::today() - lubridate::days(3)
  last_month <- function() today() - lubridate::period(months = 1)

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

alphabetise <- function(x) {
  r_match <- "\\b(in )?r\\b"
  has_r <- str_detect(x, r_match)

  out <- x %>%
    str_replace_all(r_match, "") %>%
    str_squish() %>%
    str_split(" ") %>%
    map(str_sort) %>%
    map_chr(str_flatten, " ")

  out[has_r] <- paste0(out[has_r], " [r]")
  out
}

group_terms <- function(df) {
  df %>%
    mutate(query_a = alphabetise(query)) %>%
    group_by(query_a) %>%
    summarise(
      query = list(query),
      clicks = sum(clicks),
      position = weighted.mean(position, impressions),
      impressions = sum(impressions),
    ) %>%
    select(query_a, query, clicks, impressions, position) %>%
    mutate(ctr = clicks / impressions) %>%
    arrange(desc(clicks))
}
