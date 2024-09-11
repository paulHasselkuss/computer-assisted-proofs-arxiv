source("utils.R")

## Linear model
#stats.total <- stats.total %>% mutate(percentage=matches/total)
#plot(stats.total$year, stats.total$percentage, type="b", xlab="Year", ylab="Percentage of Matches")
#lmod <- lm(percentage ~ year, data=stats.total)
#summary(lmod)
#plot(stats.total$year, stats.total$percentage, 
#      main = "Linear Regression of Percentage vs Year",
#     xlab = "Year", 
#     ylab = "Percentage of Matches",
#     pch = 19, 
#     col = "blue")
#abline(model, col = "red", lwd = 2)

## Quadratic model
#model_poly <- lm(percentage ~ poly(year, 2), data=stats.total)
#summary(model_poly)

model_logistic <- glm(cbind(matches, total-matches) ~ year, 
                       data = stats.total, 
                       family = binomial)
 
summary(model_logistic)
report(model_logistic)

stats.categories %>%
  ggplot(aes(x = year, y = matches / total)) +
    geom_point(color = "blue", size = 2) + # Observed data points
    stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Proportion of Matches vs Year",
        x = "Year", 
        y = "Proportion of Matches") +
    my_theme

stats.categories %>%
  filter(categories.primary == "math") %>%
  ggplot(aes(x = year, y = matches / total)) +
    geom_point(color = "blue", size = 2) + # Observed data points
    stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Proportion of Matches vs Year",
        x = "Year", 
        y = "Proportion of Matches") +
    my_theme

model_logistic <- glm(cbind(matches, total-matches) ~ year, 
  data = stats.categories %>%filter(categories.primary == "math"), 
  family = binomial)

summary(model_logistic)

stats.categories %>%
  filter(categories.primary == "cs") %>%
  ggplot(aes(x = year, y = matches / total)) +
    geom_point(color = "blue", size = 2) + # Observed data points
    stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
    labs(title = "Proportion of Matches vs Year",
        x = "Year", 
        y = "Proportion of Matches") +
    my_theme

model_logistic <- glm(cbind(matches, total-matches) ~ year, 
  data = stats.categories %>%filter(categories.primary == "cs"), 
  family = binomial)

summary(model_logistic)


stats.total %>%
  ggplot(aes(x=year, y=matches)) +
  geom_bar(stat = "identity",fill="#69b3a2", alpha=.9) +
  stat_smooth(method="gam", se=T, formula=y ~ s(x, bs = "cs")) +
  scale_x_continuous(limits = c(1986, 2024), breaks=seq(1986,2024, by=4)) +
  scale_y_continuous(limits = c(0, 305), breaks=seq(0,300, by=25)) +
  labs(title="Matches by year",
       subtitle = "Matching preprints across all categories on the ArXiv from 1986 to 2023", 
       x= "Year",
       y= "No. of matches") +
  my_theme_hist

model_poly <- lm(matches ~ poly(year, 2), data=stats.total)
summary(model_poly)
report(model_poly)

library(mgcv)
model_smooth <- gam(matches ~ s(year, bs = "cs"), data = stats.total)
summary(model_smooth)