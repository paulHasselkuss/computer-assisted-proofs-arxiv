source("common.R")

### THE TOTALS

statsTotal %>%
  ggplot(aes(x=year, y=matches)) +
  geom_line(color="black",size=1) +
  scale_x_continuous(limits = c(1985, 2025), breaks=seq(1985, 2025, by=5)) +
  scale_y_continuous(limits = c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(title="Matches by year",
    subtitle = "Number of matching preprints on the ArXiv from 1986 to 2024", 
    x= "Year of submission",
    y= "No. of matches") +
  theme_ipsum_rc(grid="Y")
saveImage(title="stats_total")

statsTotal %>% write_csv("out/stats_total.csv")

totalPoly <- lm(matches ~ poly(year, 2), data=statsTotal)
summary(totalPoly)

totalLogistic <- glm(cbind(matches, total-matches) ~ year, data = statsTotal, family = binomial)
summary(totalLogistic)
Anova(totalLogistic, type = "II") # X2
exp(coef(totalLogistic)["year"]) # odds

### THE CATEGORIES (MATH AND CS)

temp <- statsCollapsed %>% mutate(categories.primary.dup=categories.primary)

statsCollapsed %>%
  ggplot(aes(x=year, y=matches)) +
  geom_line( data=temp %>% dplyr::select(-categories.primary), aes(group=categories.primary.dup), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=categories.primary), color="black", linewidth=1 )+
  facet_wrap(~categories.primary, ncol=2) +
    labs(title="Matches by category and year",
       subtitle = "Number of matching preprints on the ArXiv from 1986 to 2024", 
       x= "Year of submission",
       y= "No. of matches") +
  theme_ipsum_rc(grid="Y")
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
