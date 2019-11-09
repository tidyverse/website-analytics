# remotes::install_github("josiahparry/gtrendsR", "interest_refactor")

library(tidyverse)
library(trendyy)

ob <- trendy(c("ggplot2", "dplyr"))
df <- get_interest(ob)

df %>%
  filter(hits > 0) %>%
  ggplot(aes(date, hits, colour = keyword, group = keyword)) +
  geom_line(colour = "grey80") +
  geom_smooth(se = FALSE) +
  scale_y_log10()


ob <- trendy(c("dplyr", "data.table"), from = as.Date("2010-01-01"), to = Sys.Date())
ob
df <- get_interest(ob)

df %>%
  filter(hits > 0) %>%
  ggplot(aes(date, hits, colour = keyword, group = keyword)) +
  geom_line(colour = "grey80") +
  geom_smooth(se = FALSE, span = 0.25)
