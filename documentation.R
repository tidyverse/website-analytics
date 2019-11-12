library(tidyverse)
library(lubridate)
library(shiny)
source("utils/analytics.R")
source("utils/search-console.R")

# Documentation and articles -------------------------------------------------

filter_docs <- filter_or(
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/reference/"),
  googleAnalyticsR::dim_filter("pagePath", "BEGINS_WITH", "/articles/")
)

topics_raw <- analytics(c("hostname", "pagePath"), dim_filters = filter_docs)
topics <- topics_raw %>%
  filter(is_tidyverse(hostname)) %>%
  mutate(package = package_name(hostname), hostname = NULL) %>%
  filter(package != "www") %>%
  select(package, pagePath, sessions, users) %>%
  arrange(desc(sessions))

topics

topics %>%
  filter(package == "dplyr", sessions > 500) %>%
  print(n = 20)

search_queries("dplyr.tidyverse.org/reference/join.html") %>% group_terms()

topics %>%
  filter(sessions > 100) %>%
  group_by(package) %>%
  filter(row_number() <= 10) %>%
  arrange(.by_group = TRUE) %>%
  print(n = Inf)


# Weekly trends -----------------------------------------------------------
# Not really sure what we should be looking for here

by_week <- analytics_weekly(c("hostname", "pagePath"), dim_filters = filter_docs)
pages <- by_week %>%
  filter(is_tidyverse(hostname)) %>%
  mutate(package = package_name(hostname), hostname = NULL) %>%
  filter(package != "www") %>%
  extract(pagePath, c("type", "topic"), "/(.*)/(.*)\\.html", remove = FALSE) %>%
  mutate(label = str_glue("{package}::{topic}")) %>%
  filter(!is.na(type)) %>%
  group_by(label, week) %>%
  summarise(sessions = sum(sessions), users = sum(users)) %>%
  filter(mean(sessions) > 100) %>%
  ungroup() %>%
  complete(label, week, fill = list(sessions = 1, users = 1))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("docs", "docs", choices = unique(pages$label), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("trend", height = "600px")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    req(input$docs)
    filter(pages, label %in% input$docs)
  })

  output$trend <- renderPlot({
    selected() %>%
      mutate(label = fct_reorder2(label, week, sessions)) %>%
      ggplot(aes(week, sessions, colour = label)) +
      geom_line() +
      scale_y_log10() +
      theme_grey(16)
  })
}
shinyApp(ui, server)
