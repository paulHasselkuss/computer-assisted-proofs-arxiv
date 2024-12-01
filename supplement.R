source("common.R")

statsTotal %>%
  ggplot(aes(x=year, y=total)) +
  geom_bar(stat = "identity",fill="#69b3a2", alpha=.9) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  labs(title="Submissions by year",
       subtitle = "Submitted preprints across all categories on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of submissions") +
  myThemeHist
saveImage(title="sublement_stats_submissions")

statsTotal %>%
  ggplot(aes(x=year, y=matches)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "dodgerblue3",
    se = T, # confidence intervals
    size = .5
  ) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  scale_y_continuous(limits = c(0, 305), breaks=seq(0, 300, by=25)) +
  labs(title="Matches by year",
       subtitle = "Matching preprints across all categories on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of matches") +
  myThemeHist
saveImage(title="sublement_stats_total")


statsTotal %>%
  mutate(percent = matches/total*100) %>%
  ggplot(aes(x=year, y=percent)) +
  geom_point() +
    geom_smooth(
      method = "glm",
      formula = y ~ poly(x, 2),
      color = "dodgerblue3",
      se = T, # confidence intervals
      size = .5
    ) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  labs(title="% of matches by year",
       subtitle = "Percentage of matching preprints across all categories on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "Matches from Total (%)") +
  myThemeHist
saveImage(title="sublement_stats_total_percentage")

statsCategories %>%
  filter(categories.primary == "math" | categories.primary == "cs") %>%
  ggplot(aes(x=year, y=matches, shape=categories.primary)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "dodgerblue3",
    se = T, # confidence intervals
    size = .5
  ) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  scale_y_continuous(limits = c(0, 300), breaks=seq(0, 300, by=25)) +
  labs(title="Matches by year",
       subtitle = "Matching preprints in cs and math on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of matches",
       shape="category") +
  myThemeHist
saveImage(title="sublement_stats_categories")

statsCategories %>%
  filter(categories.primary == "math" | categories.primary == "cs") %>%
  mutate(percent = matches/total*100) %>%
  ggplot(aes(x=year, y=percent, shape=categories.primary)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    formula = y ~ poly(x, 2),
    color = "dodgerblue3",
    se = T, #confidence intervals
    size = .5
  ) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  labs(title="% of matches by year",
       subtitle = "Percentage of matching preprints in cs and math on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "Matches from Total (%)",
       shape="category") +
  myThemeHist
saveImage(title="sublement_stats_categories_percentage")


categoriesToKeep <- c("math", "cs", "eess", "physics")
statsCollapsed <- statsCategories %>%
  mutate(categories.primary = ifelse(
    !str_detect(categories.primary, paste(categoriesToKeep, collapse = "|")),
    "other",
    categories.primary
  )) %>%
  group_by(year, categories.primary) %>%
  summarize(
    total = sum(total),
    matches = sum(matches),
    .groups = "drop"
  )