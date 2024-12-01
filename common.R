# Load required libraries
library(tidyverse)
library(jsonlite)
library(car)

filterYear <- 2024

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

saveImage <- function(title = "plot", p = last_plot(), x = 1, y = 1) {
  ggsave(paste0(title, ".png"), plot = p, path = "./out/", width = 1400 * x, height = 1050 * y, dpi = 200, units = "px")
  print(paste0("Image written: ", title, ".png"))
}

myTheme <- theme(
  text = element_text(family = "EB Garamond", size = 12),
  plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  axis.text = element_text(color = "black"),
  axis.text.x = element_text(size = 12, face = "bold"),
  plot.title.position = "plot",
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  panel.grid = element_line(color = "#b4aea9"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = "dashed"),
  panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
  plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
  legend.background = element_rect(fill = "#ffffff", color = "#ffffff"),
  legend.position="bottom",
  legend.box.spacing.x = unit(0, "pt")
)

myThemeHist <- myTheme + theme(
  text = element_text(size = 10),
  plot.title = element_text(size = 18),
  axis.text.x = element_text(size = 10, face = "plain"),
  axis.line = element_line(color = "black")
)
