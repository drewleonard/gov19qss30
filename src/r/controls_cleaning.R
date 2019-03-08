rm(list = ls())
setwd('~/Desktop/19W/qss_30/project/')

library(tidyverse)
library(matrixStats)

controls_clean <- read_csv('./data/controls_clean.csv')

controls_cleaner <- controls_clean %>%
  filter(COUNTYICP != 0 & county_long != "Unavailable") %>%
  rename(year = YEAR, 
         state = name_state) %>% 
  mutate(state = state.abb[match(state, state.name)]) %>% 
  group_by(year, county_long, state) %>% 
  summarize(population = sum(PERWT),
            prop_black = sum(PERWT[RACE == 2])/sum(PERWT),
            prop_white = sum(PERWT[RACE == 1 & HISPAN == 0])/sum(PERWT),
            median_income_household = matrixStats::weightedMedian(FTOTINC, HHWT),
            median_income_personal = matrixStats::weightedMedian(INCTOT, PERWT),
            median_poverty_household = matrixStats::weightedMedian(POVERTY, HHWT),
            median_poverty_personal = matrixStats::weightedMedian(POVERTY, PERWT))

write.csv(controls_cleaner, './data/controls_cleaner.csv')
