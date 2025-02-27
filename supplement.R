source("common.R")

statsTotal %>%
  ggplot(aes(x=year, y=total)) +
  geom_line(color="#69b3a2",size=1) +
  scale_x_continuous(limits = c(1985, 2025), breaks=seq(1985, 2025, by=5)) +
  labs(title="Submissions by year",
       subtitle = "Submitted preprints across all categories on the ArXiv from 1986 to 2024", 
       x= "Year",
       y= "No. of submissions") +
  theme_ipsum_rc(grid="Y")
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
  scale_x_continuous(limits = c(1986, 2025), breaks=seq(1986, 2024, by=4)) +
  labs(title="Matches by year",
       subtitle = "Matching preprints across all categories on the ArXiv from 1986 to 2024", 
       x= "Year",
       y= "No. of matches") +
  theme_ipsum_rc(grid="Y")
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
  theme_ipsum_rc(grid="Y")
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
  labs(title="Matches by year",
       subtitle = "Matching preprints in cs and math on the ArXiv from 1986 to 2024", 
       x= "Year",
       y= "No. of matches",
       shape="category") +
  theme_ipsum_rc(grid="Y")
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
       subtitle = "Percentage of matching preprints in cs and math on the ArXiv from 1986 to 2024", 
       x= "Year",
       y= "Matches from Total (%)",
       shape="category") +
        theme_ipsum_rc(grid="Y")
saveImage(title="sublement_stats_categories_percentage")