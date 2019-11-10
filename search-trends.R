library(tidyverse)
# remotes::install_github("josiahparry/gtrendsR", "interest_refactor")
library(gtrendsR)

ob <- gtrends(c("ggplot2", "dplyr"))
df <- as_tibble(ob$interest_over_time)
df %>%
  filter(hits > 0) %>%
  ggplot(aes(date, hits, colour = keyword, group = keyword)) +
  geom_line(colour = "grey80") +
  geom_smooth(se = FALSE) +
  scale_y_log10()

ob <- gtrends(c("dplyr", "data.table"), time = "all")
df <- as_tibble(ob$interest_over_time)
df$hits <- as.numeric(df$hits)
df %>%
  filter(hits > 0, date > as.Date("2014-01-01")) %>%
  ggplot(aes(date, hits, colour = keyword, group = keyword)) +
  geom_line(colour = "grey80") +
  geom_smooth(se = FALSE, span = 0.25)
