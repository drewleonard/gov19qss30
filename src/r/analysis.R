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

# Fit 1: Controlled
fit1 <- glm.nb(
  fatalities ~ controlled + noncontrolled + violent_crime + median_income_household + population + population_black,
  data = merged
)
summary(fit1)

# Fit 1
# Effect plot
pdf('./writing/figures/fit1_effects.pdf')
effect_plot(
  fit1,
  pred = controlled,
  interval = TRUE,
  x.label = "Controlled Item Expenditures (Logged)",
  y.label = "Expected Police Killings"
)
dev.off()

# Fit 2: total
fit2 <- glm.nb(
  fatalities ~ total + violent_crime + median_income_household + population + population_black,
  data = merged
)
summary(fit2)

# Plotting and exporting models
coef_names <- c(
  "Controlled Items" = "controlled",
  "Non-Controlled Items" = "noncontrolled",
  "All Items" = "total"
)
pdf('./writing/figures/fit1_fit2_effects.pdf')
plot_summs(fit1, fit2, scale = TRUE, coefs = coef_names, ci_level = 0.95, model.names = c("Controlled Items", "All Items"), legend.title = "Regression Model", colors = c("blue", "red"), inner_ci_level = 0.90)
dev.off()

# Pearson's R correlation between controlled and non-controlled items
cor(merged$controlled, merged$noncontrolled, method = "pearson")

# Stargazing
stargazer(fit1, 
          fit2, 
          no.space = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.labels = c("Controlled Items", "All Items"))

# Interaction plot
fit3 <- glm.nb(
  fatalities ~ controlled * noncontrolled + violent_crime + median_income_household + population + population_black,
  data = merged
)

pdf('./writing/figures/fit3_effects.pdf')
interact_plot(fit3, 
              pred = controlled, 
              modx = noncontrolled, 
              interval = TRUE, 
              x.label = "Controlled Item Expenditures (Logged)", 
              y.label = "Expected Police Killings", 
              legend.main = "Non-Controlled Item\nExpenditures (Logged)", 
              int.width = 0.90)
dev.off()
