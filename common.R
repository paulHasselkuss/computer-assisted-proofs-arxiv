# Load required libraries
library(tidyverse)
library(jsonlite)
library(hrbrthemes)
library(car)

filterYear <- 2025
categoriesToKeep <- c("math", "cs", "eess")

# the matching preprints + metadata
matches <- stream_in(file("processed/matches.json")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year<filterYear)

# statistics about the total number of preprints + matching preprints by YEAR
statsTotal <- stream_in(file("processed/stats_total.json")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year<filterYear) %>%
  group_by(year) %>%
  summarize(
    total = sum(total),
    matches = sum(matches),
    .groups = 'drop'  # This removes the grouping after the summarize step
  )

# statistics about the total number of preprints + matching preprints by YEAR and CATEGORY
statsCategories <- stream_in(file("processed/stats_categories.json")) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year<filterYear) %>%
  group_by(year, categories.primary) %>%
  summarize(
    total = sum(total),
    matches = sum(matches),
    .groups = 'drop'  # This removes the grouping after the summarize step
  ) #%>%
  #pivot_wider(names_from = categories.primary, values_from = c(total, matches), values_fill = 0)

# same data, but with minor categories collapsed into one
statsCollapsed <- statsCategories %>%
  mutate(categories.primary = ifelse(
    categories.primary %in% categoriesToKeep,
    categories.primary,
    "other"
  )) %>%
    group_by(year, categories.primary) %>%
    summarize(
      total = sum(total),
      matches = sum(matches),
      .groups = "drop"
    ) %>%
    mutate(
      categories.primary=as_factor(categories.primary),
      categories.primary=fct_relevel(categories.primary, c("math", "cs", "eess", "other"))
    )

saveImage <- function(title = "plot", p = last_plot(), x = 1, y = 1) {
  ggsave(paste0(title, ".png"), plot = p, path = "./out/", width = 1500 * x, height = 1100 * y, dpi = 200, units = "px")
  print(paste0("Image written: ", title, ".png"))
}

