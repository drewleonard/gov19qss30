rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(jtools)
library(MASS)
library(stargazer)
library(cowplot)
library(interactions)

# Read in merged data-set  (all four, cleaned)
merged <- read_csv('./data/analysis_ready.csv')

# Baseline Delehanty et al. (2017) models

# Controlled spending, controlling for noncontrolled spending
fit1 <- MASS::glm.nb(
  fatalities ~ controlled + noncontrolled + violent_crime + median_income_household + population_black + population,
  data = merged
)
summary(fit1)
ep1 <- effect_plot(
  fit1,
  pred = controlled,
  interval = TRUE,
  x.label = "Controlled Spending",
  y.label = "Fatalities"
)

# Total
fit2 <- MASS::glm.nb(
  fatalities ~ total + violent_crime + median_income_household + population_black + population,
  data = merged
)
summary(fit2)

# Plotting and exporting models
coef_names <- c(
  "Expenditures (Controlled)" = "controlled",
  "Expenditures (Non-Controlled)" = "noncontrolled",
  "Expenditures (Total)" = "total",
  "Violent Crime" = "violent_crime",
  "Median Income" = "median_income_household",
  "Population Black" = "population_black"
)
plot_summs(fit1, fit2, scale = TRUE, coefs = coef_names, ci_level = 0.90)

# Pearson's R correlation between controlled and non-controlled items
cor(merged$controlled, merged$noncontrolled, method = "pearson")

# Interaction plot
fit3 <- MASS::glm.nb(
  fatalities ~ controlled * noncontrolled + violent_crime + median_income_household + population_black + population,
  data = merged
)
interact_plot(fit3, pred = controlled, modx = noncontrolled, interval = TRUE)
