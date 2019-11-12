library(tidyverse)
# remotes::install_github("josiahparry/gtrendsR", "interest_refactor")
library(gtrendsR)

ob <- gtrends(c("ggplot2", "dplyr", "tidyverse"))
df <- as_tibble(ob$interest_over_time)
df %>%
  filter(hits > 0) %>%
  ggplot(aes(date, hits, colour = keyword, group = keyword)) +
  geom_line(colour = "grey80") +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  labs(
    title = "Google search trends",
    x = NULL,
    y = "Relative hits",
    colour = "Search term"
  )
