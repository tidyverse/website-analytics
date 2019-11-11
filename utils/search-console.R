library(searchConsoleR)
searchConsoleR::scr_auth(email = TRUE)

# suppress broken check
assignInNamespace("check.Url", function(url, ...) url, "searchConsoleR")

# "today" in search console time is 3 days ago

sc_get <- function(dimension, filter = NULL, to = today() - 3, duration = "1 month", row_limit = 100) {
  # dimension <- rlang::arg_match(dimension, c("query", "date", "country", "device", "page"))

  from <- to - as.period(duration)

  out <- suppressMessages(searchConsoleR::search_analytics(
    "sc-domain:tidyverse.org",
    dimensions = dimension,
    dimensionFilterExp = filter,
    startDate = from,
    endDate = to,
    rowLimit = row_limit
  ))

  tibble::as_tibble(out)
}

search_queries <- function(url, to = today() - 3, duration = "1 month") {
  filter <- paste0("page~~", url)
  sc_get(dimension = "query", filter = filter, to = to, duration = duration)
}

search_pages <- function(query, to = today() - 3, duration = "1 month") {
  filter <- paste0("query==", query)
  sc_get(dimension = "page", filter = filter, to = to, duration = duration)
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
