source("utils.R")

### THE TOTALS

statsTotal %>%
  ggplot(aes(x=year, y=matches)) +
  geom_bar(stat = "identity",fill="#69b3a2", alpha=.9) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986, 2024, by=4)) +
  scale_y_continuous(limits = c(0, 305), breaks=seq(0, 300, by=25)) +
  labs(title="Matches by year",
       subtitle = "Matching preprints across all categories on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of matches") +
  myThemeHist
saveImage(title="stats_total")

statsTotal %>% write_csv("out/stats_total.csv")

totalPoly <- lm(matches ~ poly(year, 2), data=statsTotal)
summary(totalPoly)

totalLogistic <- glm(cbind(matches, total-matches) ~ year, data = statsTotal, family = binomial)
summary(totalLogistic)
Anova(totalLogistic, type = "II") # X2
exp(coef(totalLogistic)["year"]) # odds

### THE CATEGORIES (MATH AND CS)

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

statsCollapsed %>%
  mutate(
    categories.primary=as_factor(categories.primary),
    categories.primary=fct_relevel(categories.primary, c("math", "cs", "eess", "physics", "other"))
  ) %>%
  ggplot( aes(x=year, y=matches, fill=categories.primary)) +
  geom_bar(stat = "identity", alpha=.9) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986,2024, by=4)) +
  scale_y_continuous(breaks=seq(0,600, by=50)) +
  labs(title="Matches by category and year",
       subtitle = "Matching preprints on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of matches",
       fill="") +
  myThemeHist

saveImage(title="stats_categories")

statsCollapsed %>%
  pivot_wider(names_from = categories.primary, values_from = c(total, matches), values_fill = 0) %>%
  write_csv("out/stats_categories.csv")

statsMath <- statsCategories %>% filter(categories.primary == "math")

mathPoly <- lm(matches ~ poly(year, 2), data=statsMath)
summary(mathPoly)

mathLogistic <- glm(cbind(matches, total-matches) ~ year, data = statsMath, family = binomial)
summary(mathLogistic)
Anova(mathLogistic, type = "II") # X2
exp(coef(mathLogistic)["year"]) # odds

statsCs <- statsCategories %>% filter(categories.primary == "cs")

csPoly <- lm(matches ~ poly(year, 2), data=statsCs)
summary(csPoly)

csLogistic <- glm(cbind(matches, total-matches) ~ year, data = statsCs, family = binomial)
summary(csLogistic)
Anova(csLogistic, type = "II") # X2
exp(coef(csLogistic)["year"]) # odds

## KEYWORDS

matches %>% count(match) %>% write_csv("out/stats_keywords.csv")
