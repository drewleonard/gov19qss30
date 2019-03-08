rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(jtools)
library(MASS)
library(stargazer)
library(cowplot)

merged <- read_csv('./data/analysis_ready.csv')

# Get data for controlled items only
merged_controlled <- merged %>%
  filter(group_small != "Unavailable") %>%
  mutate(controlled = ifelse(group_small == "Office", "Uncontrolled", "Controlled"))

# Regressing
# Baseline Delehanty et al. (2017) model
# OLS
fitt1 <- lm(
  fatalities ~ expenditure_value_logged + violent_crime + median_income_household + population + population_black,
  data = merged
)
summary(fitt1)
effect_plot(
  fitt1,
  pred = expenditure_value_logged,
  interval = TRUE,
  x.label = "Expenditure Value (logged)",
  y.label = "Fatalities"
)

# Baseline Delehanty et al. (2017) model
# Negative binomial regression
fitt2 <- MASS::glm.nb(
  fatalities ~ expenditure_value_logged + violent_crime + median_income_household + population_black + population,
  data = merged
)
summary(fitt2)
effect_plot(
  fitt2,
  pred = expenditure_value_logged,
  interval = TRUE,
  x.label = "Expenditure Value (logged)",
  y.label = "Fatalities"
)

# Basline
# Controlled only, ols
fitt3 <- lm(
  fatalities ~ expenditure_value_logged + violent_crime + median_income_household + population_black + population,
  data = merged_controlled
)
summary(fitt3)
effect_plot(
  fitt3,
  pred = expenditure_value_logged,
  interval = TRUE,
  x.label = "Expenditure Value (logged)",
  y.label = "Fatalities"
)

# Basline
# Controlled only, nbr
fitt4 <- MASS::glm.nb(
  fatalities ~ expenditure_value_logged + violent_crime + median_income_household + population_black + population,
  data = merged_controlled
)
summary(fitt4)
effect_plot(
  fitt4,
  pred = expenditure_value_logged,
  interval = TRUE,
  x.label = "Expenditure Value (logged)",
  y.label = "Fatalities"
)

# Two-way anova shows that controlled and not-controlled do not matter
fitt5 <- aov(
  fatalities ~ expenditure_value_logged * controlled + violent_crime + median_income_household + population_black + population,
  data = merged_controlled
)
summary(fitt5)

# Plotting and exporting models
coef_names <- c(
  "Expenditures (logged)" = "expenditure_value_logged",
  "Violent crime" = "violent_crime",
  "Median income" = "median_income_household",
  "Population black" = "population_black"
)
plot_summs(fitt1, fitt2, fitt4, scale = TRUE, transform.response = TRUE, coefs = coef_names, ci_level = 0.90)
# export_summs(fitt1, fitt2, fitt4, scale = TRUE, coefs = coef_names, ci_level = 0.90)
